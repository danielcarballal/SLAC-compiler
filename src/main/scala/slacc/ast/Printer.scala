package slacc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    var kind = t.getClass.getSimpleName
    var tr = t.getClass
    var ret = ""
    t match {
    	case Program(main, classes) =>
    		for(clas <- classes){
    			ret += Printer(clas)
    			ret += "\n"
    		}
    		ret += Printer(main)
    	case MainMethod(main) =>
    		ret += Printer(main)
    	case ClassDecl(id, parent, vars, methods) =>
    		ret += "class "
    		ret += Printer(id)
    		if(!parent.isEmpty){
    			ret += " <: "
    			ret += Printer(parent.get)
    		}
    		ret += "{\n"
    		for(v <- vars) { ret += Printer(v); ret += "\n" }
    		for(m <- methods) { ret += Printer(m); ret += "\n"}
    		ret += "}"
    	case VarDecl(typeOf, id) =>
    		ret += "var "
    		ret += Printer(id)
    		ret += " : "
    		ret += Printer(typeOf)
    		ret += ";"
    	case MethodDecl(retType, id, args, vars, exprs, retExpr) =>
    		ret += "method "
    		ret += Printer(id)
    		ret += "("
    		for(arg <- args){ ret += Printer(arg) }
    		ret += "):"
			ret += Printer(retType)
			ret += "={\n"
			for(vara <- vars) { ret += Printer(vara); ret+= "\n"}
			for(expr <- exprs) { ret += Printer(expr); ret+= ";\n"}
			ret = ret.dropRight(2)
			ret += "\n}"
    	case Formal(tpe, id) =>
    		ret += Printer(id)
    		ret += " = "
    		ret += Printer(tpe)
    	case IntArrayType() =>
    		ret = "Int[]"
    	case IntType() =>
    		ret = "Int"
    	case BooleanType() =>
    		ret = "Boolean"
    	case StringType() =>
    		ret = "String"
    	case UnitType() =>
    		ret = "Unit"
    	case And(lhs, rhs) =>
    		ret += Printer(lhs)
    		ret += " && "
    		ret += Printer(rhs)
    	case Or(lhs, rhs) =>
    		ret += Printer(lhs)
    		ret += " || "
    		ret += Printer(rhs)
    	case Plus(lhs, rhs) =>
    		ret += Printer(lhs)
    		ret += " + "
    		ret += Printer(rhs)
    	case Minus(rhs, lhs) =>
    		ret += Printer(lhs)
    		ret += " - "
    		ret += Printer(rhs)
    	case Times(lhs, rhs) =>
    		ret += Printer(lhs)
    		ret += " * "
    		ret += Printer(rhs)
    	case Div(rhs, lhs) =>
    		ret += Printer(lhs)
    		ret += " / "
    		ret += Printer(rhs)
    	case LessThan(rhs, lhs) =>
    		ret += Printer(lhs)
    		ret += " < "
    		ret += Printer(rhs)
    	case Equals(rhs, lhs) =>
    		ret += Printer(lhs)
    		ret += " == "
    		ret += Printer(rhs)
    	case ArrayRead(arr, index) =>
    		ret += Printer(arr)
    		ret += "["
    		ret += Printer(index)
    		ret += "]"
    	case ArrayLength(arr) =>
    		ret += Printer(arr)
    		ret += ".length "
    	case MethodCall(obj, meth, args) =>
    		ret += Printer(obj)
    		ret += "."
    		ret += Printer(meth)
    		ret += "("
    		for(arg <- args) {ret += Printer(arg); ret += ",";} //TODO, eliminate last comma
    		ret = ret.dropRight(1)

    	case IntLit(value) =>
    		ret = value + ""
    	case StringLit(value) =>
    		ret = value
    	case True() =>
    		ret = "True"
    	case False() =>
    		ret = "False"
    	case Identifier(value) =>
    		ret = value
    	case Self() =>
    		ret = "self"
    	case NewIntArray(size) =>
    		ret = "new Int ["
    		ret += Printer(size)
    		ret += "]"
    	case New(expr) =>
    		ret += "new "
    		ret += Printer(expr)
    		ret += "()"
    	case Not(expr) =>
    		ret += "!"
    		ret += Printer(expr)
    	case Block(list) => 
    		ret += "{"
    		for(item <- list){ ret += item; ret += ";\n"}//Todo, eliminate last ;
    		ret = ret.dropRight(2) // Drop the last semicolon
    		ret += "\n}"
    	case If(expr, thn, els) =>
    		ret += "if("
    		ret += Printer(expr)
    		ret += ")\n{\n"
			ret += Printer(thn)
			ret += "}\n"
			if(els != None){
				ret += "else{\n"
				ret += Printer(els.get)
				ret += "}"
			}
    	case While(cond, body) =>
    		ret += "while("
    		ret += Printer(cond)
    		ret += ")\n{\n"
			ret += Printer(body)
			ret += "\n}\n"
    	case Println(expr) =>
    		ret += "println("
    		ret += Printer(expr)
    		ret += ")"
    	case Assign(id, expr) =>
    		ret += Printer(id)
    		ret += " = "
    		ret += Printer(expr)

    	case ArrayAssign(id, index, expr) =>
    		ret += Printer(id)
    		ret == "["
    		ret += Printer(index)
    		ret += "]"
    		ret += " = "
    		ret += Printer(expr)
    	case _ =>
    }
    ret
  }
}
