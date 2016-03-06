package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next
        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def skip(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    def parseGoal: Program = {

      println("START PARSING " + currentToken.kind);
      var classlist: List[ClassDecl] = Nil
      while(currentToken.kind == CLASS){ classlist = classlist :+ classdefinition; readToken; }
      var mainmeth = mainmethoddelceration
      Program(mainmeth, classlist)
    }

    def classdefinition: ClassDecl ={
      //class Identifier ( <: Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
      //case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      skip(CLASS)
      var id = identifier 
      var parent: Option[Identifier] = None
      if(currentToken.kind == LESSTHAN){
        readToken
        skip(COLON)
        parent = Some(identifier)
      }
      skip(LBRACE)
      var varList: List[VarDecl] = Nil
      while(currentToken.kind == VAR){ varList :+ vardecleration }
      var methList: List[MethodDecl] = Nil
      while(currentToken.kind == METHOD){ methList :+ methoddecleration}
      ClassDecl(id, parent, varList, methList );
    }

    def vardecleration: VarDecl = {
      println("Var declaration " + currentToken.kind)
      //var Identifier : Type ;
      skip(VAR)
      var id = identifier
      skip(COLON)
      var t = typedeclaration
      skip(SEMICOLON)

      VarDecl(t, id)
    }

    def mainmethoddelceration: MainMethod = {
      //method Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = { ( VarDeclaration )* Expression ( ; Expression )* }
      var md = methoddecleration
      if(md.id == Identifier("Main")){
        return MainMethod(md)
      }
      println("Error: Main expected")
      MainMethod(md)

    }

    //method Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = { ( VarDeclaration )* Expression ( ; Expression )* }
    //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree)
    def methoddecleration: MethodDecl = {
      println("START METHOD")
      skip(METHOD)
      var methname = identifier
      skip(LPAREN)
      var args: List[Formal] = Nil
      if(currentToken.kind == IDKIND){
        var firstId: Identifier = identifier
        skip(COLON)
        var firstType: TypeTree = typedeclaration
        args :+ Formal(firstType, firstId)
        while(currentToken.kind == COMMA){
          readToken
          var id: Identifier = identifier
          skip(COLON)
          var stype: TypeTree = typedeclaration
          args :+ Formal(stype, id)
        }
      }
      skip(RPAREN)
      skip(COLON)
      var retType = typedeclaration
      skip(EQSIGN)

      skip(LBRACE)

      var varList: List[VarDecl] = Nil
      while(currentToken.kind == VAR) { varList :+ vardecleration; readToken; }
      println(currentToken.kind)
      var latestExpr = expr 
      var exprList: List[ExprTree] = List(latestExpr);
      println("First expression of method " + latestExpr)
      readToken
      while(currentToken.kind == SEMICOLON) { latestExpr = expr; println("Adding expr " + latestExpr); exprList :+ latestExpr;  readToken; }

      skip(RBRACE)
      MethodDecl(retType, methname, args, varList, exprList, latestExpr)
    }

    def typedeclaration: TypeTree = {
      var typeRet: TypeTree = new BooleanType();

      currentToken.kind match{
       case BOOLEAN =>
        typeRet = BooleanType()
       case STRING => 
        typeRet =  StringType()
       case UNIT =>
        typeRet =  UnitType();
       case INT =>
          readToken
          if(currentToken.kind == LBRACKET){
            skip(RBRACKET)
            typeRet =  IntArrayType()
          } else {
            typeRet = IntType()
          }
        case _ =>
          println(currentToken + " is not a valid type!")
      }
      typeRet
    }

    def expr: ExprTree = {
      println("Finding expression " + currentToken.kind)
      var ret: ExprTree = simpleexpr
      while(currentToken.kind == BANG){
        skip(BANG)
        var exprnew: ExprTree = expr
        ret = Not(exprnew)
      }
      while(currentToken.kind == TIMES){
        skip(TIMES)
        var exprnew: ExprTree = expr
        ret = Times(ret, exprnew)
      }
      while(currentToken.kind == DIV){
        skip(DIV)
        var exprnew: ExprTree = expr
        ret = Div(ret, exprnew)
      }
      while(currentToken.kind == PLUS){
        skip(PLUS)
        var exprnew: ExprTree = expr
        ret = Plus(ret, exprnew)
      }
      while(currentToken.kind == MINUS){
        skip(MINUS)
        var exprnew: ExprTree = expr
        ret = Minus(ret, exprnew)
      }
      while(currentToken.kind == LESSTHAN){
        skip(LESSTHAN)
        var exprnew: ExprTree = expr
        ret = LessThan(ret, exprnew)
      }
      while(currentToken.kind == EQUALS){
        skip(EQUALS)
        var exprnew: ExprTree = expr
        ret = Equals(ret, exprnew)
      }
      while(currentToken.kind == AND){
        skip(AND)
        var exprnew: ExprTree = expr
        ret = And(ret, exprnew)
      }
      while(currentToken.kind == OR){
        skip(OR)
        var exprnew: ExprTree = expr
        ret = Or(ret, exprnew)
      }
      while(currentToken.kind == LBRACKET){
        skip(LBRACKET)
        var exprnew: ExprTree = expr
        skip(RBRACKET)
        ret = ArrayRead(ret, exprnew)
      }
      while(currentToken.kind == DOT){
        skip(DOT)
        if(currentToken.kind == LENGTH){
          skip(LENGTH)
          var exprnew: ExprTree = expr
          ret = ArrayLength(ret)
        }
        else{
          var id = identifier
          skip(LPAREN)
          var exprList: List[ExprTree] = Nil
          if(currentToken.kind != RPAREN){ //At least one arg
            exprList :+ expr
            while(currentToken.kind == COMMA){
              skip(COMMA)
              exprList :+ expr
            }
            ret = MethodCall(ret, id, exprList)
          }
        }
      }
      ret
    }

    def simpleexpr: ExprTree = {
      println("Simple expr " + currentToken.kind)
      var retVal: ExprTree = True() //placeholder
      currentToken.kind match {
        case INTLITKIND =>
          retVal = IntLit(555)
          skip(INTLITKIND)
        case STRLITKIND =>
          retVal = StringLit("Placeholder")
          skip(STRLITKIND)
        case TRUE =>
          skip(TRUE)
          retVal = True()
        case FALSE =>
          skip(FALSE)
          retVal = False()
        case SELF =>
          skip(SELF)
          retVal = Self()
        case LPAREN =>
          skip(LPAREN)
          expr
          skip(RPAREN)

        case NEW =>
          skip(NEW)
          if(currentToken.kind == INT){
            skip(INT)
            skip(LBRACKET)
            var ex = expr
            skip(RBRACKET)
            retVal = NewIntArray(ex)
          } else if(currentToken.kind == IDKIND){
            var id = identifier
            skip(LPAREN); skip(RPAREN)
            retVal = New(id)
          }
        case IF =>
          skip(IF)
          skip(LPAREN)
          var condition = expr

        case LBRACE =>

        case WHILE =>

        case PRINTLN =>

        case STROF =>

        case _ => //Should be identifier

      }
      retVal
    }

    def identifier: Identifier = {
      println("Inside identifier ")
      var id = Identifier(currentToken.toString)
      skip(IDKIND)
      id
    }


    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
