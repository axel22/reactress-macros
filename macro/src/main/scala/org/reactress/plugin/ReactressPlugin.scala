package org.reactress
package plugin



import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform.Transform



class ReactressPlugin(val global: Global) extends Plugin {
  import global._

  val name = "reactress"
  val description = "generates reactive getters and setters"
  val components = List[PluginComponent](AddMuxComponent, RewritesComponent)
  
  val reactiveClass = definitions.getClass(newTermName(classOf[Reactive].getName))
  val reactAnnotSimpleName = newTypeName(classOf[react].getSimpleName)
  val reactAnnotName = newTypeName(classOf[react].getName)
  val reactAnnotation = definitions.getClass(reactAnnotName)
  val mux0Class = definitions.getClass(newTermName(classOf[Mux0[_]].getName))
  val mux1Class = definitions.getClass(newTermName(classOf[Mux1[_, _]].getName))
  val mux2Class = definitions.getClass(newTermName(classOf[Mux2[_, _, _]].getName))
  val mux3Class = definitions.getClass(newTermName(classOf[Mux3[_, _, _, _]].getName))
  val mux4Class = definitions.getClass(newTermName(classOf[Mux4[_, _, _, _, _]].getName))
  val mux0Module = mux0Class.companionSymbol
  val mux1Module = mux1Class.companionSymbol
  val mux2Module = mux2Class.companionSymbol
  val mux3Module = mux3Class.companionSymbol
  val mux4Module = mux4Class.companionSymbol
  val observeName = newTermName("observe")

  private object AddMuxComponent extends PluginComponent with Transform {
    val global: ReactressPlugin.this.global.type = ReactressPlugin.this.global
    override val runsAfter = List("parser")
    override val runsBefore = List("namer")
    val phaseName = ReactressPlugin.this.name + "-addmux"

    import global._

    def newTransformer(unit: CompilationUnit) = new ReactressTransformer(unit)

    class ReactressTransformer(unit: CompilationUnit) extends Transformer {
      import Flag._

      def transformTemplate(classname: Name, tparams: List[TypeDef], parents: List[Tree], self: ValDef, body: List[Tree]): Template = {
        val nbody = for (member <- body) yield member match {
          case ValDef(mods, name, tpe, rhs) if mods.hasAnnotationNamed(reactAnnotSimpleName) =>
            val muxname = newTermName(name + "$mux")
            val mux = atPos(member.pos) {
              val tpetree = TypeTree()
              val init = TypeApply(Select(Ident(mux0Module), newTermName("None")), List(Ident(classname)))
              ValDef(mods.copy(annotations = Nil), muxname, tpetree, init)
            }
            List(member, mux)
          case DefDef(mods, name, tps, vps, tpe, rhs) if mods.hasAnnotationNamed(reactAnnotSimpleName) =>
            val muxname = newTermName(name + "$mux")
            def flatType(tree: Tree, tps: List[TypeDef]): Tree = if (tps.isEmpty) tree else AppliedTypeTree(tree, tps.map(t => flatType(Ident(t.name), t.tparams)))
            val mux = atPos(member.pos) {
              val tpetree = TypeTree()
              val init = vps.flatten match {
                case Nil =>
                  TypeApply(Select(Ident(mux1Module), newTermName("None")), List(flatType(Ident(classname), tparams), tpe))
                case ValDef(_, _, tp1, _) :: Nil =>
                  TypeApply(Select(Ident(mux2Module), newTermName("None")), List(flatType(Ident(classname), tparams), tp1, tpe))
                case ValDef(_, _, tp1, _) :: ValDef(_, _, tp2, _) :: Nil =>
                  TypeApply(Select(Ident(mux3Module), newTermName("None")), List(flatType(Ident(classname), tparams), tp1, tp2, tpe))
                case ValDef(_, _, tp1, _) :: ValDef(_, _, tp2, _) :: ValDef(_, _, tp3, _) :: Nil =>
                  TypeApply(Select(Ident(mux4Module), newTermName("None")), List(flatType(Ident(classname), tparams), tp1, tp2, tp3, tpe))
                case _ =>
                  unit.error(member.pos, "Only support reactive methods with up to 3 parameters.")
                  EmptyTree
              }
              ValDef(Modifiers(MUTABLE), muxname, tpetree, init)
            }
            val wrappername = newTermName(name + "$reactive")
            val wrapper = atPos(member.pos) {
              val select = 
                if (tps.length > 0) TypeApply(Select(This(newTypeName(classname.toString)), name.toString), tps)
                else Select(This(newTypeName(classname.toString)), name.toString)
              val application = vps.foldLeft(select) {
                (acc, vp) => Apply(acc, vp.map(v => Ident(v.name)))
              }
              val appresname = newTermName(name + "$reactive$0")
              val nrhs = Block(
                List(
                  ValDef(Modifiers(), appresname, TypeTree(), application),
                  Apply(Select(Select(This(classname.toString), muxname), newTermName("dispatch")), This(classname.toString) :: vps.flatten.map(v => Ident(v.name)) ::: List(Ident(appresname)))
                ),
                Ident(appresname)
              )
              DefDef(mods.copy(annotations = Nil), wrappername, tps, vps, tpe, nrhs)
            }
            List(member, mux, wrapper)
          case _ =>
            List(member)
        }

        val ntemplate = Template(parents, self, nbody.flatten)
        super.transform(ntemplate).asInstanceOf[Template]
      }

      override def transform(tree: Tree): Tree = tree match {
        case ModuleDef(mods, name, Template(parents, self, body)) =>
          ModuleDef(mods, name, transformTemplate(name, Nil, parents, self, body))
        case ClassDef(mods, name, tparams, Template(parents, self, body)) =>
          ClassDef(mods, name, tparams, transformTemplate(name, tparams, parents, self, body))
        case t =>
          super.transform(t)
      }
    }

  }

  private object RewritesComponent extends PluginComponent with Transform {
    val global: ReactressPlugin.this.global.type = ReactressPlugin.this.global
    override val runsAfter = List("refchecks")
    val phaseName = ReactressPlugin.this.name + "-rewrites"

    import global._

    def newTransformer(unit: CompilationUnit) = new ReactressTransformer

    class ReactressTransformer extends Transformer {
      import Flag._
      import reflect.internal.Flags.{getterFlags, setterFlags}

      def isReactiveCall(target: Tree, method: TermName) = if (target.symbol == null) false else {
        val m = target.symbol.info.member(method)
        def notInWrapper = {
          val wrapper = m.owner.info.member(newTermName(method + "$reactive"))
          currentMethod != wrapper
        }
        m.isMethod && !m.isAccessor && m.hasAnnotation(reactAnnotation) && notInWrapper
      }

      override def transform(tree: Tree): Tree = tree match {
        case impl @ Template(parents, self, body) if currentClass.tpe <:< reactiveClass.tpe =>
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
        case Apply(TypeApply(Select(target, method), tparams), params) if isReactiveCall(target, method) =>
          val wrappername = method + "$reactive"
          val localTyper = typer.atOwner(tree, tree.symbol.owner)
          val napply = localTyper.typed {
            atPos(tree.pos) {
              Apply(TypeApply(Select(target, wrappername), tparams), params)
            }
          }
          napply
        case Apply(Select(target, method), params) if isReactiveCall(target, method) =>
          val wrappername = method + "$reactive"
          val localTyper = typer.atOwner(tree, tree.symbol.owner)
          val napply = localTyper.typed {
            atPos(tree.pos) {
              Apply(Select(target, wrappername), params)
            }
          }
          napply
        case t =>
          super.transform(t)
      }
    }
    
  }

}