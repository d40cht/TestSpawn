import scala.collection.{mutable, immutable}

// Paul P:

/*
   Initial focus: lazy vals, by-name parameters, closures, anything defined in a method, anything defined in an anonymous class, anything with default arguments, anything which uses the pattern matcher, anything with structural types.
   
   Notes:
   
 - many combinations would be invalid scala.  This is fine - how the compiler handles invalid code is no less important.
 - clearly it wouldn't be hard to take n^n far enough that exhaustion becomes impossible.  That's fine too; the next step would be to have a filtering mechanism so one can use a predicate to reduce the space to more interesting cases.
 -  you could get a long and super useful already ways just doing something like I did in the primitive version, smooshing strings together.  But it gets too hard to test anything nontrivial, like "this type parameter has that type parameter as one of its bounds and then..." but many of our bugs require things to refer to one another like that.
 - one could consider doing this with real scala AST (DefDef, ClassDef, etc) rather than generating source from scratch.  I don't like that idea because I think it falls to "regulatory capture" - by assuming the language of the compiler, one becomes blind to whole categories of bugs.
*/


// ( for (amod <- List("private", "protected", "") ; mod <- Set("final", "sealed").subsets ; kind <- List("trait", "class", "object") ; member <- List("def", "var", "val")) yield (List(amod :: mod.toList mkString " ", kind, "C {", member, "x = super.x }") mkString " " trim) ).sorted.distinct foreach println

// Some stuff here: http://brenocon.com/scalacheat/

// Writer for outputting 
class SyntaxSink
{
    var indent = 0
    var doIndent = true
    
    def push( token : String )
    {
        if ( doIndent )
        {
            print( " " * (indent*4) )
            doIndent = false
        }
        print( token + " " )
    }
    
    def eol()
    {
        print( "\n" )
        doIndent = true
    }
    
    def inScope( more : => Unit )
    {
        eol()
        push("{")
        eol()
        indent += 1
        more
        indent -= 1
        push("}")
        eol()
    }
}


trait SyntaxGenerator
{
    def output( s : SyntaxSink )
}


// Reflecting the Scala type hierarchy
trait AnyTypeType extends SyntaxGenerator

// AnyVal
trait AnyValType extends AnyTypeType
case class DoubleType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Double" ) } }
case class FloatType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Float" ) } }
case class LongType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Long" ) } }
case class IntType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Int" ) } }
case class ShortType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Short" ) } }
case class ByteType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Byte" ) } }
case class CharType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Char" ) } }
case class BooleanType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Boolean" ) } }
case class UnitType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Unit" ) } }

// AnyRef
trait AnyRef extends AnyTypeType

trait AccessModifierTag extends SyntaxGenerator
case class PrivateAccessTag extends AccessModifierTag { def output( s : SyntaxSink ) { s.push( "private" ) } }
case class ProtectedAccessTag extends AccessModifierTag { def output( s : SyntaxSink ) { s.push( "protected" ) } }
case class PublicAccessTag extends AccessModifierTag { def output( s : SyntaxSink ) { s.push( "public" ) } }


class TypeModifierTag( val isFinal : Boolean, val isSealed : Boolean ) extends SyntaxGenerator
{
    def output( s : SyntaxSink )
    {
        if ( isFinal ) s.push( "final" )
        if ( isSealed ) s.push( "sealed" )
    }
}


// case class too
trait TypeKindTag extends SyntaxGenerator
case class TraitKindTag extends TypeKindTag { def output( s : SyntaxSink ) { s.push( "trait" ) } }
case class ObjectKindTag extends TypeKindTag { def output( s : SyntaxSink ) { s.push( "object" ) } }
case class ClassKindTag extends TypeKindTag { def output( s : SyntaxSink ) { s.push( "class" ) } }
case class AbstractClassKindTag extends TypeKindTag { def output( s : SyntaxSink ) { s.push( "abstract class" ) } }

trait ValueModifierTag extends SyntaxGenerator
case class ValValueTag extends ValueModifierTag { def output( s : SyntaxSink ) { s.push( "val" ) } }
case class VarValueTag extends ValueModifierTag { def output( s : SyntaxSink ) { s.push( "var" ) } }
case class NoneValueTag extends ValueModifierTag { def output( s : SyntaxSink ) {} }

trait ExprType extends SyntaxGenerator
case class ConstantExpr( val value : Double ) extends ExprType
{
    def output( s : SyntaxSink )
    {
        s.push( value.toString )
    }
}

// TODO: Fold in member function definitions
class ValueDefinition( val name : String, val modifier : ValueModifierTag, val vType : Option[AnyTypeType], val expression : Option[ExprType] )
{
    def output( s : SyntaxSink )
    {
        // val blah : Double = 12.0
        modifier.output(s)
        s.push(name)
        vType.foreach( v =>
        {
            s.push(":")
            v.output(s)
        } )
        expression.foreach( e =>
        {
            s.push("=")
            e.output(s)
        } )
        s.eol()
    }
}

// TODO: inheritance (extends and with)
// TODO: type parameters (generics with all the sorts of type constraints etc.)
class ConcreteType( val name : Option[String], val accessModifier : AccessModifierTag, val modifier : TypeModifierTag, val kind : TypeKindTag ) extends AnyRef
{
    var valueDefinitions = mutable.ArrayBuffer[ValueDefinition]()
    def output( s : SyntaxSink )
    {
        // private sealed abstract class Blah( val a : Double, val b : Int )
        // {
        //    val q = a + b
        // }
        
        accessModifier.output(s)
        modifier.output(s)
        kind.output(s)
        name.foreach( n => s.push(n) )
        s inScope
        {
            valueDefinitions.foreach( v => v.output(s) )
        }
    }
}

object Main extends scala.App
{
    override def main(args : Array[String])
    {
        val s = new SyntaxSink()
        val ct = new ConcreteType( Some("Blah"), new PrivateAccessTag(), new TypeModifierTag( isFinal=true, isSealed=false ), new ClassKindTag() )
        // ValueDefinition( val name : String, val modifier : ValueModifierTag, val vType : Option[AnyTypeType], val expression : Option[ExprType] )
        ct.valueDefinitions.append( new ValueDefinition( "a", new VarValueTag(), Some( new DoubleType() ), None ) )
        ct.valueDefinitions.append( new ValueDefinition( "b", new VarValueTag(), Some( new DoubleType() ), Some( new ConstantExpr(3.0) ) ) )
        ct.valueDefinitions.append( new ValueDefinition( "c", new ValValueTag(), None, Some( new ConstantExpr(5.0) ) ) )
        ct.output(s)
    }
}


