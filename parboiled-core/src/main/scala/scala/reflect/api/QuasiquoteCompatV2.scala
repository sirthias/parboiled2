package scala.reflect.api

// QuasiquoteCompat is sort of semi-synthetic
// therefore also keep an eye on reflect/Definitions.scala where it's materialized

object QuasiquoteCompatV2 { def apply(u0: Universe): QuasiquoteCompatV2 { val u: u0.type } = new { val u: u0.type = u0 } with QuasiquoteCompatV2 }
trait QuasiquoteCompatV2 {
  val u: Universe
  import u._, definitions._, Flag._

  // ==================== NEW APIS INTRODUCED IN UNIVERSE ====================

  object TermName {
    def apply(s: String): TermName = newTermName(s)
    def unapply(name: TermName): Some[String] = Some(name.toString)
  }

  object TypeName {
    def apply(s: String): TypeName = newTypeName(s)
    def unapply(name: TypeName): Some[String] = Some(name.toString)
  }

  object Modifiers {
    def apply(flags: FlagSet, privateWithin: Name = TermName(""), annotations: List[Tree] = Nil): Modifiers =
      u.Modifiers(flags, privateWithin, annotations)
    def unapply(mods: Modifiers): Some[(FlagSet, Name, List[Tree])] =
      Some((mods.flags, mods.privateWithin, mods.annotations))
  }

  object EmptyValDefLike {
    def unapply(tree: Tree): Boolean = tree eq emptyValDef
  }

  // ==================== NEW APIS INTRODUCED IN BUILDUTILS ====================

  object RefTree {
    def apply(qualifier: Tree, name: Name): RefTree = qualifier match {
      case EmptyTree ⇒
        Ident(name)
      case qual if qual.isTerm ⇒
        Select(qual, name)
      case qual if qual.isType ⇒
        assert(name.isTypeName, s"qual = $qual, name = $name")
        SelectFromTypeTree(qual, name.toTypeName)
    }
    def apply(qual: Tree, sym: Symbol): Tree = apply(qual, sym.name).setSymbol(sym)
    def unapply(refTree: RefTree): Option[(Tree, Name)] = Some((refTree.qualifier, refTree.name))
  }

  def mkAnnotation(tree: Tree): Tree = tree match {
    case SyntacticNew(Nil, SyntacticApplied(SyntacticTypeApplied(_, _), _) :: Nil, emptyValDef, Nil) ⇒
      tree
    case _ ⇒
      throw new IllegalArgumentException(s"Tree ${showRaw(tree)} isn't a correct representation of annotation." +
        """Consider reformatting it into a q"new $name[..$targs](...$argss)" shape""")
  }

  def mkAnnotation(trees: List[Tree]): List[Tree] = trees.map(mkAnnotation)

  def mkVparamss(argss: List[List[ValDef]]): List[List[ValDef]] = argss.map(_.map(mkParam))

  def mkParam(vd: ValDef): ValDef = {
    var newmods = (vd.mods | PARAM) & (~DEFERRED)
    if (vd.rhs.nonEmpty) newmods |= DEFAULTPARAM
    copyValDef(vd)(mods = newmods)
  }

  def mkTparams(tparams: List[Tree]): List[TypeDef] =
    tparams.map {
      case td: TypeDef ⇒ copyTypeDef(td)(mods = (td.mods | PARAM) & (~DEFERRED))
      case other       ⇒ throw new IllegalArgumentException("can't splice $other as type parameter")
    }

  def mkRefineStat(stat: Tree): Tree = {
    stat match {
      case dd: DefDef  ⇒ require(dd.rhs.isEmpty, "can't use DefDef with non-empty body as refine stat")
      case vd: ValDef  ⇒ require(vd.rhs.isEmpty, "can't use ValDef with non-empty rhs as refine stat")
      case td: TypeDef ⇒
      case _           ⇒ throw new IllegalArgumentException(s"not legal refine stat: $stat")
    }
    stat
  }

  def mkRefineStat(stats: List[Tree]): List[Tree] = stats.map(mkRefineStat)

  object ScalaDot {
    def apply(name: Name): Tree = gen.scalaDot(name)
    def unapply(tree: Tree): Option[Name] = tree match {
      case Select(id @ Ident(SCALA), name) if id.symbol == ScalaPackage ⇒ Some(name)
      case _ ⇒ None
    }
  }

  def mkEarlyDef(defn: Tree): Tree = defn match {
    case vdef @ ValDef(mods, _, _, _) if !mods.hasFlag(DEFERRED) ⇒
      copyValDef(vdef)(mods = mods | PRESUPER)
    case tdef @ TypeDef(mods, _, _, _) ⇒
      copyTypeDef(tdef)(mods = mods | PRESUPER)
    case _ ⇒
      throw new IllegalArgumentException(s"not legal early def: $defn")
  }

  def mkEarlyDef(defns: List[Tree]): List[Tree] = defns.map(mkEarlyDef)

  object FlagsRepr {
    def apply(bits: Long): FlagSet = bits.asInstanceOf[FlagSet]
    def unapply(flags: Long): Some[Long] = Some(flags)
  }

  object SyntacticTypeApplied {
    def apply(tree: Tree, targs: List[Tree]): Tree =
      if (targs.isEmpty) tree
      else if (tree.isTerm) TypeApply(tree, targs)
      else if (tree.isType) AppliedTypeTree(tree, targs)
      else throw new IllegalArgumentException(s"can't apply types to $tree")

    def unapply(tree: Tree): Some[(Tree, List[Tree])] = tree match {
      case TypeApply(fun, targs)       ⇒ Some((fun, targs))
      case AppliedTypeTree(tpe, targs) ⇒ Some((tpe, targs))
      case _                           ⇒ Some((tree, Nil))
    }
  }

  object SyntacticApplied {
    def apply(tree: Tree, argss: List[List[Tree]]): Tree =
      argss.foldLeft(tree) { Apply(_, _) }

    def unapply(tree: Tree): Some[(Tree, List[List[Tree]])] = {
      val treeInfo.Applied(fun, targs, argss) = tree
      Some((SyntacticTypeApplied(fun, targs), argss))
    }
  }

  private object UnCtor {
    def unapply(tree: Tree): Option[(Modifiers, List[List[ValDef]], List[List[Tree]], List[Tree])] = tree match {
      case DefDef(mods, MIXIN_CONSTRUCTOR, _, _, _, Block(lvdefs, _)) ⇒
        Some((mods | Flag.TRAIT, Nil, Nil, lvdefs))
      case DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, _, Block(lvdefs :+ SyntacticApplied(_, argss), _)) ⇒
        Some((mods, vparamss, argss, lvdefs))
      case _ ⇒ None
    }
  }

  private object UnMkTemplate {
    def unapply(templ: Template): Option[(List[Tree], ValDef, Modifiers, List[List[ValDef]], List[Tree], List[Tree])] = {
      val Template(parents0, selfdef, tbody) = templ
      def result(ctorMods: Modifiers, vparamss: List[List[ValDef]], edefs: List[Tree], parents: List[Tree], body: List[Tree]) =
        Some((parents, selfdef, ctorMods, vparamss, edefs, body))
      def indexOfCtor(trees: List[Tree]) =
        trees.indexWhere { case UnCtor(_, _, _, _) ⇒ true; case _ ⇒ false }

      if (tbody forall treeInfo.isInterfaceMember)
        result(NoMods | Flag.TRAIT, Nil, Nil, parents0, tbody)
      else if (indexOfCtor(tbody) == -1)
        None
      else {
        val (rawEdefs, rest) = tbody.span(treeInfo.isEarlyDef)
        val (gvdefs, etdefs) = rawEdefs.partition(treeInfo.isEarlyValDef)
        val (fieldDefs, UnCtor(ctorMods, ctorVparamss, argss, lvdefs) :: body) = rest.splitAt(indexOfCtor(rest))
        val parents = parents0 match {
          case head :: tail ⇒
            if (argss == List(Nil)) parents0
            else SyntacticApplied(head, argss) :: tail
          case Nil ⇒
            Nil
        }
        val evdefs = gvdefs.zip(lvdefs).map {
          case (gvdef @ ValDef(_, _, tpt: TypeTree, _), ValDef(_, _, _, rhs)) ⇒
            copyValDef(gvdef)(tpt = tpt.original, rhs = rhs)
          case (gvdef @ ValDef(_, _, tpt, _), ValDef(_, _, _, rhs)) ⇒
            copyValDef(gvdef)(tpt = tpt, rhs = rhs)
        }
        val edefs = evdefs ::: etdefs
        if (ctorMods.hasFlag(TRAIT))
          result(ctorMods, Nil, edefs, parents, body)
        else {
          // undo conversion from (implicit ... ) to ()(implicit ... ) when its the only parameter section
          val vparamssRestoredImplicits = ctorVparamss match {
            case Nil :: (tail @ ((head :: _) :: _)) if head.mods.hasFlag(IMPLICIT) ⇒ tail
            case other ⇒ other
          }
          // undo flag modifications by mergeing flag info from constructor args and fieldDefs
          val modsMap = fieldDefs.map { case ValDef(mods, name, _, _) ⇒ name -> mods }.toMap
          val vparamss = vparamssRestoredImplicits.map {
            _.map { vd ⇒
              val originalMods = modsMap(vd.name) | (vd.mods.flags & DEFAULTPARAM)
              atPos(vd.pos)(ValDef(originalMods, vd.name, vd.tpt, vd.rhs))
            }
          }
          result(ctorMods, vparamss, edefs, parents, body)
        }
      }
    }
  }

  object SyntacticClassDef {
    def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
              constrMods: Modifiers, vparamss: List[List[ValDef]], earlyDefs: List[Tree],
              parents: List[Tree], selfdef: ValDef, body: List[Tree]): ClassDef = {
      val extraFlags = PARAMACCESSOR | (if (mods.hasFlag(CASE)) CASEACCESSOR else NoFlags)
      val vparamss0 = vparamss.map { _.map { vd ⇒ copyValDef(vd)(mods = (vd.mods | extraFlags) & (~DEFERRED)) } }
      val tparams0 = mkTparams(tparams)
      val parents0 = gen.mkParents(mods,
        if (mods.hasFlag(CASE)) parents.filter {
          case ScalaDot(PRODUCT | SERIALIZABLE | ANYREF) ⇒ false
          case _                                         ⇒ true
        }
        else parents
      )
      val body0 = earlyDefs ::: body
      val templ = gen.mkTemplate(parents0, selfdef, constrMods, vparamss0, body0)
      gen.mkClassDef(mods, name, tparams0, templ)
    }

    def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]], List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
      case ClassDef(mods, name, tparams, UnMkTemplate(parents, selfdef, ctorMods, vparamss, earlyDefs, body)) if !ctorMods.hasFlag(TRAIT) && !ctorMods.hasFlag(JAVA) ⇒
        Some((mods, name, tparams, ctorMods, vparamss, earlyDefs, parents, selfdef, body))
      case _ ⇒
        None
    }
  }

  object SyntacticTraitDef {
    def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef], earlyDefs: List[Tree],
              parents: List[Tree], selfdef: ValDef, body: List[Tree]): ClassDef = {
      val mods0 = mods | TRAIT | ABSTRACT
      val templ = gen.mkTemplate(parents, selfdef, Modifiers(TRAIT), Nil, earlyDefs ::: body)
      gen.mkClassDef(mods0, name, mkTparams(tparams), templ)
    }

    def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
      case ClassDef(mods, name, tparams, UnMkTemplate(parents, selfdef, ctorMods, vparamss, earlyDefs, body)) if mods.hasFlag(TRAIT) ⇒
        Some((mods, name, tparams, earlyDefs, parents, selfdef, body))
      case _ ⇒ None
    }
  }

  object SyntacticModuleDef {
    def apply(mods: Modifiers, name: TermName, earlyDefs: List[Tree],
              parents: List[Tree], selfdef: ValDef, body: List[Tree]) =
      ModuleDef(mods, name, gen.mkTemplate(parents, selfdef, NoMods, Nil, earlyDefs ::: body))

    def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
      case ModuleDef(mods, name, UnMkTemplate(parents, selfdef, _, _, earlyDefs, body)) ⇒
        Some((mods, name, earlyDefs, parents, selfdef, body))
      case _ ⇒
        None
    }
  }

  private trait ScalaMemberRef {
    val symbols: Seq[Symbol]
    def result(name: Name): Option[Symbol] =
      symbols.collect { case sym if sym.name == name ⇒ sym }.headOption
    def unapply(tree: Tree): Option[Symbol] = tree match {
      case id @ Ident(name) if symbols.contains(id.symbol) && name == id.symbol.name ⇒
        Some(id.symbol)
      case Select(scalapkg @ Ident(SCALA), name) if scalapkg.symbol == ScalaPackage ⇒
        result(name)
      case Select(Select(Ident(nme.ROOTPKG), SCALA), name) ⇒
        result(name)
      case _ ⇒ None
    }
  }
  private object TupleClassRef extends ScalaMemberRef {
    val symbols = TupleClass.filter { _ != null }.toSeq
  }
  private object TupleCompanionRef extends ScalaMemberRef {
    val symbols = TupleClassRef.symbols.map { _.companionModule }
  }
  private object UnitClassRef extends ScalaMemberRef {
    val symbols = Seq(UnitClass)
  }
  private object FunctionClassRef extends ScalaMemberRef {
    val symbols = FunctionClass.toSeq
  }

  object SyntacticTuple {
    def apply(args: List[Tree]): Tree = args match {
      case Nil ⇒ Literal(Constant(()))
      case _ ⇒
        require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
        Apply(Ident(TupleClass(args.length).companionModule), args)
    }

    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case Literal(Constant(())) ⇒
        Some(Nil)
      case Apply(TupleCompanionRef(sym), args) if args.length <= MaxTupleArity
        && sym == TupleClass(args.length).companionModule ⇒
        Some(args)
      case _ ⇒
        None
    }
  }

  object SyntacticTupleType {
    def apply(args: List[Tree]): Tree = args match {
      case Nil ⇒ u.Select(u.Ident(SCALA), UNIT)
      case _ ⇒
        require(args.length <= MaxTupleArity, s"Tuples with arity bigger than $MaxTupleArity aren't supported")
        AppliedTypeTree(Ident(TupleClass(args.length)), args)
    }

    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case UnitClassRef(_) ⇒
        Some(Nil)
      case AppliedTypeTree(TupleClassRef(sym), args) if args.length <= MaxTupleArity && sym == TupleClass(args.length) ⇒
        Some(args)
      case _ ⇒
        None
    }
  }

  object SyntacticFunctionType {
    def apply(argtpes: List[Tree], restpe: Tree): Tree = {
      require(argtpes.length <= MaxFunctionArity + 1, s"Function types with arity bigger than $MaxFunctionArity aren't supported")
      gen.mkFunctionTypeTree(argtpes, restpe)
    }

    def unapply(tree: Tree): Option[(List[Tree], Tree)] = tree match {
      case AppliedTypeTree(FunctionClassRef(sym), args @ (argtpes :+ restpe)) if args.length - 1 <= MaxFunctionArity && sym == FunctionClass(args.length - 1) ⇒
        Some((argtpes, restpe))
      case _ ⇒ None
    }
  }

  object SyntacticBlock {
    def apply(stats: List[Tree]): Tree = gen.mkBlock(stats)

    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case u.Block(stats, expr) ⇒ Some(stats :+ expr)
      case _ if tree.isTerm     ⇒ Some(tree :: Nil)
      case _                    ⇒ None
    }
  }

  object SyntacticFunction {
    def apply(params: List[ValDef], body: Tree): Tree = {
      val params0 = params.map { arg ⇒
        require(arg.rhs.isEmpty, "anonymous functions don't support default values")
        mkParam(arg)
      }
      Function(params0, body)
    }

    def unapply(tree: Tree): Option[(List[ValDef], Tree)] = tree match {
      case Function(params, body) ⇒ Some((params, body))
      case _                      ⇒ None
    }
  }

  object SyntacticNew {
    def apply(earlyDefs: List[Tree], parents: List[Tree], selfdef: ValDef, body: List[Tree]): Tree =
      gen.mkNew(parents, selfdef, earlyDefs ::: body, NoPosition, NoPosition)

    def unapply(tree: Tree): Option[(List[Tree], List[Tree], ValDef, List[Tree])] = tree match {
      case SyntacticApplied(Select(New(SyntacticTypeApplied(ident, targs)), nme.CONSTRUCTOR), argss) ⇒
        Some((Nil, SyntacticApplied(SyntacticTypeApplied(ident, targs), argss) :: Nil, emptyValDef, Nil))
      case SyntacticBlock(SyntacticClassDef(_, ANON_CLASS_NAME, Nil, _, List(Nil), earlyDefs, parents, selfdef, body) ::
        Apply(Select(New(Ident(ANON_CLASS_NAME)), nme.CONSTRUCTOR), Nil) :: Nil) ⇒
        Some((earlyDefs, parents, selfdef, body))
      case _ ⇒
        None
    }
  }

  object SyntacticDefDef {
    def apply(mods: Modifiers, name: TermName, tparams: List[Tree], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef =
      DefDef(mods, name, mkTparams(tparams), mkVparamss(vparamss), tpt, rhs)

    def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[List[ValDef]], Tree, Tree)] = tree match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) ⇒ Some((mods, name.toTermName, tparams, vparamss, tpt, rhs))
      case _ ⇒ None
    }
  }

  trait SyntacticValDefBase {
    val isMutable: Boolean

    def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree) = {
      val mods1 = if (isMutable) mods | MUTABLE else mods
      ValDef(mods1, name, tpt, rhs)
    }

    def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)] = tree match {
      case ValDef(mods, name, tpt, rhs) if mods.hasFlag(MUTABLE) == isMutable ⇒
        Some((mods, name, tpt, rhs))
      case _ ⇒
        None
    }
  }

  object SyntacticValDef extends SyntacticValDefBase { val isMutable = false }
  object SyntacticVarDef extends SyntacticValDefBase { val isMutable = true }

  object SyntacticAssign {
    def apply(lhs: Tree, rhs: Tree): Tree = gen.mkAssign(lhs, rhs)
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case Assign(lhs, rhs)                       ⇒ Some((lhs, rhs))
      case Apply(Select(fn, UPDATE), args :+ rhs) ⇒ Some((atPos(fn.pos)(Apply(fn, args)), rhs))
      case _                                      ⇒ None
    }
  }

  // ==================== TRICKY IMPLEMENTATIONS ====================

  object treeInfo {
    class Applied(val tree: Tree) {
      def callee: Tree = {
        def loop(tree: Tree): Tree = tree match {
          case Apply(fn, _) ⇒ loop(fn)
          case tree         ⇒ tree
        }
        loop(tree)
      }

      def core: Tree = callee match {
        case TypeApply(fn, _)       ⇒ fn
        case AppliedTypeTree(fn, _) ⇒ fn
        case tree                   ⇒ tree
      }

      def targs: List[Tree] = callee match {
        case TypeApply(_, args)       ⇒ args
        case AppliedTypeTree(_, args) ⇒ args
        case _                        ⇒ Nil
      }

      def argss: List[List[Tree]] = {
        def loop(tree: Tree): List[List[Tree]] = tree match {
          case Apply(fn, args) ⇒ loop(fn) :+ args
          case _               ⇒ Nil
        }
        loop(tree)
      }

      override def toString = {
        val tstr = if (targs.isEmpty) "" else targs.mkString("[", ", ", "]")
        val astr = argss map (args ⇒ args.mkString("(", ", ", ")")) mkString ""
        s"$core$tstr$astr"
      }
    }

    object Applied {
      def apply(tree: Tree): treeInfo.Applied = new treeInfo.Applied(tree)

      def unapply(applied: treeInfo.Applied): Option[(Tree, List[Tree], List[List[Tree]])] =
        Some((applied.core, applied.targs, applied.argss))

      def unapply(tree: Tree): Option[(Tree, List[Tree], List[List[Tree]])] =
        unapply(new treeInfo.Applied(tree))
    }

    def dissectApplied(tree: Tree) = new Applied(tree)

    def isEarlyDef(tree: Tree) = tree match {
      case TypeDef(mods, _, _, _) ⇒ mods hasFlag PRESUPER
      case ValDef(mods, _, _, _)  ⇒ mods hasFlag PRESUPER
      case _                      ⇒ false
    }

    def isEarlyValDef(tree: Tree) = tree match {
      case ValDef(mods, _, _, _) ⇒ mods hasFlag PRESUPER
      case _                     ⇒ false
    }

    def isEarlyTypeDef(tree: Tree) = tree match {
      case TypeDef(mods, _, _, _) ⇒ mods hasFlag PRESUPER
      case _                      ⇒ false
    }

    /**
     * Is tree legal as a member definition of an interface?
     */
    def isInterfaceMember(tree: Tree): Boolean = tree match {
      case EmptyTree                    ⇒ true
      case Import(_, _)                 ⇒ true
      case TypeDef(_, _, _, _)          ⇒ true
      case DefDef(mods, _, _, _, _, __) ⇒ mods hasFlag DEFERRED
      case ValDef(mods, _, _, _)        ⇒ mods hasFlag DEFERRED
      case _                            ⇒ false
    }

    /** Is name a variable name? */
    def isVariableName(name: Name): Boolean = {
      val first = name.toString()(0)
      (((first.isLower && first.isLetter) || first == '_')
        && (name != FALSE)
        && (name != TRUE)
        && (name != NULL)
      )
    }

    /** Is tree a variable pattern? */
    def isVarPattern(pat: Tree): Boolean = pat match {
      case x: Ident ⇒ !x.isBackquoted && isVariableName(x.name)
      case _        ⇒ false
    }
  }

  def copyValDef(tree: Tree)(
    mods: Modifiers = null,
    name: Name = null,
    tpt: Tree = null,
    rhs: Tree = null): ValDef = tree match {
    case ValDef(mods0, name0, tpt0, rhs0) ⇒
      treeCopy.ValDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (tpt eq null) tpt0 else tpt,
        if (rhs eq null) rhs0 else rhs
      )
    case t ⇒
      sys.error("Not a ValDef: " + t + "/" + t.getClass)
  }
  def copyTypeDef(tree: Tree)(
    mods: Modifiers = null,
    name: Name = null,
    tparams: List[TypeDef] = null,
    rhs: Tree = null): TypeDef = tree match {
    case TypeDef(mods0, name0, tparams0, rhs0) ⇒
      treeCopy.TypeDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (tparams eq null) tparams0 else tparams,
        if (rhs eq null) rhs0 else rhs
      )
    case t ⇒
      sys.error("Not a TypeDef: " + t + "/" + t.getClass)
  }

  implicit class RichSymbol(sym: Symbol) {
    def companionModule = {
      val internalSym = sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol]
      internalSym.companionModule.asInstanceOf[Symbol]
    }
  }

  implicit class RichMods(mods: Modifiers) {
    def |(other: FlagSet) = Modifiers(mods.flags | other, mods.privateWithin, mods.annotations)
    def &(other: FlagSet) = Modifiers(mods.flags & other, mods.privateWithin, mods.annotations)
    def &~(other: FlagSet) = Modifiers(mods.flags &~ other, mods.privateWithin, mods.annotations)
    def withAnnotations(annots: List[Tree]) = Modifiers(mods.flags, mods.privateWithin, annots ::: mods.annotations)
  }

  implicit class RichFlags(flags: FlagSet) {
    def &(other: FlagSet) = (flags.asInstanceOf[Long] & other.asInstanceOf[Long]).toLong.asInstanceOf[FlagSet]
    def &~(other: FlagSet) = (flags.asInstanceOf[Long] & ~other.asInstanceOf[Long]).toLong.asInstanceOf[FlagSet]
    def unary_~() = (~flags.asInstanceOf[Long]).toLong.asInstanceOf[FlagSet]
  }

  implicit class RichTree[T <: Tree](tree: T) {
    def nonEmpty = tree != EmptyTree
    def setSymbol(sym: Symbol): T = build.setSymbol(tree, sym)
    def setType(tpe: Type): T = build.setType(tree, tpe)
  }

  implicit class RichIdent(tree: Ident) {
    def isBackquoted = {
      val symtab = u.asInstanceOf[scala.reflect.internal.SymbolTable]
      val symtabtree = tree.asInstanceOf[symtab.Tree]
      symtabtree.attachments.get[symtab.BackquotedIdentifierAttachment.type].isDefined
    }
  }

  object gen {
    def rootId(name: Name) = Select(Ident(nme.ROOTPKG), name)
    def rootScalaDot(name: Name) = Select(rootId(SCALA) setSymbol ScalaPackage, name)
    def scalaDot(name: Name) = Select(Ident(SCALA) setSymbol ScalaPackage, name)
    def scalaAnyRefConstr = scalaAnyRefConstrRaw setSymbol AnyRefClass // used in ide
    def scalaAnyRefConstrRaw = scalaDot(ANYREF)

    def mkSuperInitCall: Select = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)

    /**
     * Generates a template with constructor corresponding to
     *
     *  constrmods (vparams1_) ... (vparams_n) preSuper { presupers }
     *  extends superclass(args_1) ... (args_n) with mixins { self => body }
     *
     *  This gets translated to
     *
     *  extends superclass with mixins { self =>
     *    presupers' // presupers without rhs
     *    vparamss   // abstract fields corresponding to value parameters
     *    def <init>(vparamss) {
     *      presupers
     *      super.<init>(args)
     *    }
     *    body
     *  }
     */
    def mkTemplate(parents: List[Tree], self: ValDef, constrMods: Modifiers,
                   vparamss: List[List[ValDef]], body: List[Tree], superPos: Position = NoPosition): Template = {
      /* Add constructor to template */

      // create parameters for <init> as synthetic trees.
      var vparamss1 = vparamss.map {
        _.map { vd ⇒
          atPos(vd.pos.focus) {
            val mods = Modifiers(vd.mods.flags & (IMPLICIT | DEFAULTPARAM | BYNAMEPARAM) | PARAM | PARAMACCESSOR)
            ValDef(mods withAnnotations vd.mods.annotations, vd.name, vd.tpt.duplicate, vd.rhs.duplicate)
          }
        }
      }
      val (edefs, rest) = body span treeInfo.isEarlyDef
      val (evdefs, etdefs) = edefs partition treeInfo.isEarlyValDef
      val gvdefs = evdefs map {
        case vdef @ ValDef(_, _, tpt, _) ⇒
          copyValDef(vdef)(
            // can't use typetree wrapper here, have to resort to plain duplication
            tpt = atPos(vdef.pos.focus)(tpt.duplicate),
            rhs = EmptyTree
          )
      }
      val lvdefs = evdefs collect { case vdef: ValDef ⇒ copyValDef(vdef)(mods = vdef.mods | PRESUPER) }

      val (parents1, argss) =
        if (constrMods.hasFlag(TRAIT)) (parents, Nil)
        else {
          val SyntacticApplied(parent, argss) = parents.head
          val argss1 = if (argss.isEmpty) List(Nil) else argss
          (parent :: parents.tail, argss1)
        }

      val constr = {
        if (constrMods.hasFlag(TRAIT)) {
          if (body forall treeInfo.isInterfaceMember) None
          else Some(
            atPos(wrappingPos(superPos, lvdefs))(
              DefDef(NoMods, MIXIN_CONSTRUCTOR, List(), List(Nil), TypeTree(), Block(lvdefs, Literal(Constant(()))))))
        } else {
          // convert (implicit ... ) to ()(implicit ... ) if its the only parameter section
          if (vparamss1.isEmpty || !vparamss1.head.isEmpty && vparamss1.head.head.mods.hasFlag(IMPLICIT))
            vparamss1 = List() :: vparamss1
          val superRef: Tree = atPos(superPos)(mkSuperInitCall)
          val superCall = (superRef /: argss)(Apply.apply)
          Some(
            // TODO: previously this was `wrappingPos(superPos, lvdefs ::: argss.flatten)`
            // is it going to be a problem that we can no longer include the `argss`?
            atPos(wrappingPos(superPos, lvdefs))(
              DefDef(constrMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), Block(lvdefs ::: List(superCall), Literal(Constant(()))))))
        }
      }
      // FIXME: cant't really check this without internal api
      // constr foreach (ensureNonOverlapping(_, parents1 ::: gvdefs, focus=false))

      // Field definitions for the class - remove defaults.
      val fieldDefs = vparamss.flatten map (vd ⇒ copyValDef(vd)(mods = vd.mods &~ DEFAULTPARAM, rhs = EmptyTree))

      u.Template(parents1, self, gvdefs ::: fieldDefs ::: constr ++: etdefs ::: rest)
    }

    def mkParents(ownerMods: Modifiers, parents: List[Tree], parentPos: Position = NoPosition) =
      if (ownerMods.hasFlag(CASE)) parents ::: List(scalaDot(PRODUCT), scalaDot(SERIALIZABLE))
      else if (parents.isEmpty) atPos(parentPos)(scalaAnyRefConstrRaw) :: Nil
      else parents

    def mkClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], templ: Template): ClassDef = {
      val isInterface = mods.hasFlag(TRAIT) && (templ.body forall treeInfo.isInterfaceMember)
      val mods1 = if (isInterface) (mods | INTERFACE) else mods
      ClassDef(mods1, name, tparams, templ)
    }

    /**
     * Create positioned tree representing an object creation <new parents { stats }
     *  @param npos  the position of the new
     *  @param cpos  the position of the anonymous class starting with parents
     */
    def mkNew(parents: List[Tree], self: ValDef, stats: List[Tree],
              npos: Position, cpos: Position): Tree =
      if (parents.isEmpty)
        mkNew(List(scalaAnyRefConstr), self, stats, npos, cpos)
      else if (parents.tail.isEmpty && stats.isEmpty) {
        // `Parsers.template` no longer differentiates tpts and their argss
        // e.g. `C()` will be represented as a single tree Apply(Ident(C), Nil)
        // instead of parents = Ident(C), argss = Nil as before
        // this change works great for things that are actually templates
        // but in this degenerate case we need to perform postprocessing
        val app = treeInfo.dissectApplied(parents.head)
        atPos(npos union cpos) { New(app.callee, app.argss) }
      } else {
        val x = ANON_CLASS_NAME
        atPos(npos union cpos) {
          Block(
            List(
              atPos(cpos) {
                ClassDef(
                  Modifiers(FINAL), x, Nil,
                  mkTemplate(parents, self, NoMods, List(Nil), stats, cpos.focus))
              }),
            atPos(npos) {
              New(
                atPos(npos.focus)(Ident(x)),
                Nil)
            }
          )
        }
      }

    /** Create a tree representing the function type (argtpes) => restpe */
    def mkFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree =
      AppliedTypeTree(rootScalaDot(newTypeName("Function" + argtpes.length)), argtpes ::: List(restpe))

    /** Create block of statements `stats`  */
    def mkBlock(stats: List[Tree]): Tree =
      if (stats.isEmpty) Literal(Constant(()))
      else if (!stats.last.isTerm) Block(stats, Literal(Constant(())))
      else if (stats.length == 1) stats.head
      else Block(stats.init, stats.last)

    /** Create a tree representing an assignment <lhs = rhs> */
    def mkAssign(lhs: Tree, rhs: Tree): Tree = lhs match {
      case Apply(fn, args) ⇒
        Apply(atPos(fn.pos)(Select(fn, UPDATE)), args :+ rhs)
      case _ ⇒
        Assign(lhs, rhs)
    }

    def mkTreeOrBlock(stats: List[Tree]) = stats match {
      case Nil         ⇒ EmptyTree
      case head :: Nil ⇒ head
      case _           ⇒ gen.mkBlock(stats)
    }
  }

  // ==================== PRIVATE APIS FROM TREEBUILDER ====================

  /**
   * Convert all occurrences of (lower-case) variables in a pattern as follows:
   *    x                  becomes      x @ _
   *    x: T               becomes      x @ (_: T)
   */
  object patvarTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(name) if (treeInfo.isVarPattern(tree) && name != nme.WILDCARD) ⇒
        atPos(tree.pos)(Bind(name, atPos(tree.pos.focus)(Ident(nme.WILDCARD))))
      case Typed(id @ Ident(name), tpt) if (treeInfo.isVarPattern(id) && name != nme.WILDCARD) ⇒
        atPos(tree.pos.withPoint(id.pos.point)) {
          Bind(name, atPos(tree.pos.withStart(tree.pos.point)) {
            Typed(Ident(nme.WILDCARD), tpt)
          })
        }
      case Apply(fn @ Apply(_, _), args) ⇒
        treeCopy.Apply(tree, transform(fn), transformTrees(args))
      case Apply(fn, args) ⇒
        treeCopy.Apply(tree, fn, transformTrees(args))
      case Typed(expr, tpt) ⇒
        treeCopy.Typed(tree, transform(expr), tpt)
      case Bind(name, body) ⇒
        treeCopy.Bind(tree, name, transform(body))
      case Alternative(_) | Star(_) ⇒
        super.transform(tree)
      case _ ⇒
        tree
    }
  }

  // ==================== DEPRECATED FROM PUBLIC API ====================

  /**
   * Factory method for object creation `new tpt(args_1)...(args_n)`
   *  A `New(t, as)` is expanded to: `(new t).<init>(as)`
   */
  def New(tpt: Tree, argss: List[List[Tree]]): Tree = argss match {
    case Nil        ⇒ ApplyConstructor(tpt, Nil)
    case xs :: rest ⇒ rest.foldLeft(ApplyConstructor(tpt, xs): Tree)(Apply.apply)
  }

  def ApplyConstructor(tpt: Tree, args: List[Tree]) = Apply(Select(u.New(tpt), nme.CONSTRUCTOR), args)

  // ==================== CONSTANTS ====================

  val MaxFunctionArity, MaxTupleArity = 22

  lazy val MIXIN_CONSTRUCTOR = TermName("$init$")
  lazy val SCALA = TermName("scala")
  lazy val UPDATE = TermName("update")
  lazy val FALSE = TermName("false")
  lazy val TRUE = TermName("true")
  lazy val NULL = TermName("null")

  lazy val ANYREF = TypeName("AnyRef")
  lazy val ANON_CLASS_NAME = TypeName("$anon")
  lazy val PRODUCT = TypeName("Product")
  lazy val SERIALIZABLE = TypeName("Serializable")
  lazy val UNIT = TypeName("Unit")

  val JAVA = (1 << 20).toLong.asInstanceOf[FlagSet]
  val CASEACCESSOR = (1 << 24).toLong.asInstanceOf[FlagSet]
  val PARAMACCESSOR = (1 << 29).toLong.asInstanceOf[FlagSet]
}