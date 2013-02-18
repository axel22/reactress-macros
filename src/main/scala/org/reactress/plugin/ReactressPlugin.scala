package org.reactress
package plugin



import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform.Transform



class ReactressPlugin(val global: Global) extends Plugin {
  import global._

  val name = "reactress"
  val description = "generates reactive getters and setters"
  val components = List[PluginComponent](AddMuxComponent, SettersComponent)
  
  val reactiveStructClass = definitions.getClass(newTermName(classOf[Reactive.Struct].getName))
  val reactAnnotSimpleName = newTypeName(classOf[react].getSimpleName)
  val reactAnnotName = newTypeName(classOf[react].getName)
  val reactAnnotation = definitions.getClass(reactAnnotName)
  val mux0Class = definitions.getClass(newTermName(classOf[Mux0].getName))
  val mux0Module = mux0Class.companionSymbol

  private object AddMuxComponent extends PluginComponent with Transform {
    val global: ReactressPlugin.this.global.type = ReactressPlugin.this.global
    override val runsAfter = List("parser")
    override val runsBefore = List("namer")
    val phaseName = ReactressPlugin.this.name + "-addmux"

    import global._

    def newTransformer(unit: CompilationUnit) = new ReactressTransformer

    class ReactressTransformer extends Transformer {
      import Flag._

      override def transform(tree: Tree): Tree = tree match {
        case Template(parents, self, body) =>
          val nbody = for (member <- body) yield member match {
            case ValDef(mods, name, tpe, rhs) if mods.hasAnnotationNamed(reactAnnotSimpleName) =>
              val muxname = newTermName(name + "$mux")
              val mux = atPos(member.pos) {
                ValDef(mods.copy(annotations = Nil), muxname, TypeTree(mux0Class.tpe), Select(Ident(mux0Module), newTermName("None")))
              }

              List(member, mux)
            case m =>
              List(m)
          }

          val ntemplate = Template(parents, self, nbody.flatten)
          super.transform(ntemplate)
        case t =>
          super.transform(t)
      }
    }

  }

  private object SettersComponent extends PluginComponent with Transform {
    val global: ReactressPlugin.this.global.type = ReactressPlugin.this.global
    override val runsAfter = List("refchecks")
    val phaseName = ReactressPlugin.this.name + "-setters"

    import global._

    def newTransformer(unit: CompilationUnit) = new ReactressTransformer

    class ReactressTransformer extends Transformer {
      import Flag._
      import reflect.internal.Flags.{getterFlags, setterFlags}

      override def transform(tree: Tree): Tree = tree match {
        case impl @ Template(parents, self, body) if currentClass.tpe <:< reactiveStructClass.tpe =>
          val clazz = impl.symbol.owner
          val localTyper = typer.atOwner(impl, clazz)

          val nbody = for (member <- body) yield member match {
            case DefDef(mods, name, tps, vps, tpt, rhs) if member.symbol.isSetter && member.symbol.accessed.hasAnnotation(reactAnnotation) =>
              val field = member.symbol.accessed
              val getter = field.getter(field.owner)
              val muxname = newTermName(getter.name + "$mux")

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

              List(nmember)
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