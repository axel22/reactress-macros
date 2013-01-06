package org.reactress
package plugin



import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform.Transform



class ReactressPlugin(val global: Global) extends Plugin {
  import global._

  val name = "reactress"
  val description = "generates reactive getters and setters"
  val components = List[PluginComponent](Component)
  
  private object Component extends PluginComponent with Transform {
    val global: ReactressPlugin.this.global.type = ReactressPlugin.this.global
    val runsAfter = List[String]("refchecks")
    val phaseName = ReactressPlugin.this.name

    import global._

    val reactiveClass = definitions.getClass(newTermName(classOf[Reactive].getName))
    val reactAnnotation = definitions.getClass(newTermName(classOf[react].getName))
    val muxClass = definitions.getClass(newTermName(classOf[Mux].getName))
    val muxModule = muxClass.companionSymbol

    def newTransformer(unit: CompilationUnit) = new ReactressTransformer

    class ReactressTransformer extends Transformer {
      import Flag._
      import reflect.internal.Flags.{getterFlags, setterFlags}

      override def transform(tree: Tree): Tree = tree match {
        case impl @ Template(parents, self, body) if parents.exists(_.symbol.isSubClass(reactiveClass)) =>
          val clazz = impl.symbol.owner
          val localTyper = typer.atOwner(impl, clazz)

          def addAccessor(sym: Symbol, name: TermName, flags: Long) = {
            val m = clazz.newMethod(name, sym.pos, flags & ~(LOCAL | PRIVATE)) setPrivateWithin clazz
            clazz.info.decls enter m
          }

          def addGetter(sym: Symbol): (Symbol, Tree) = {
            val getr = addAccessor(sym, nme.getterName(sym.name.toTermName), getterFlags(sym.flags))
            getr setInfo MethodType(List(), sym.tpe)
            (getr, localTyper.typedPos(sym.pos)(DefDef(getr, Select(This(clazz), sym))))
          }

          def addSetter(sym: Symbol): (Symbol, Tree) = {
            sym setFlag MUTABLE
            val setr = addAccessor(sym, nme.getterToSetter(nme.getterName(sym.name.toTermName)), setterFlags(sym.flags))
            setr setInfo MethodType(setr.newSyntheticValueParams(List(sym.tpe)), definitions.UnitClass.tpe)
            val tree = localTyper.typed {
              atPos(sym.pos) {
                DefDef(setr, paramss =>
                  Assign(Select(This(clazz), sym), Ident(paramss.head.head)))
              }
            }
            (setr, tree)
          }

          val nbody = for (member <- body) yield member match {
            case DefDef(mods, name, tps, vps, tpt, rhs) if member.symbol.isSetter && member.symbol.accessed.hasAnnotation(reactAnnotation) =>
              // emit mux field
              val field = member.symbol.accessed
              val getter = field.getter(field.owner)
              val muxname = newTermName(getter.name + "$mux")
              val muxsym = clazz.newVariable(muxname, field.pos, FINAL | MUTABLE)
              clazz.info.decls enter muxsym
              muxsym setInfo muxClass.tpe
              val mux = localTyper.typed {
                atPos(field.pos) {
                  ValDef(muxsym, Select(Ident(muxModule), newTermName("None")))
                }
              }
              val (_, muxgetr) = addGetter(muxsym)
              val (_, muxsetr) = addSetter(muxsym)

              // find getter and instrument its body
              val nrhs = Block(
                List(
                  Apply(
                    Select(Select(This(clazz), muxname), newTermName("dispatch")),
                    List(This(clazz))
                  )
                ),
                rhs
              )
              val nsetter = DefDef(mods, name, tps, vps, tpt, nrhs)
              nsetter.symbol = member.symbol

              val nmember = localTyper.typed {
                atPos(field.pos) {
                  nsetter
                }
              }

              List(nmember, mux, muxgetr, muxsetr)
            case m =>
              List(m)
          }

          val ntemplate = Template(parents, self, nbody.flatten)
          ntemplate.symbol = tree.symbol
          ntemplate.tpe = tree.tpe
          super.transform(ntemplate)
        case t =>
          super.transform(t)
      }
    }
  }

}