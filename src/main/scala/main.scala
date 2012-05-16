import scala.collection.{mutable, immutable}

// Paul P:

/*
   
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
    
    def commaSeparated( els : Seq[SyntaxGenerator] )
    {
        var first = true
        els.foreach( e =>
        {
            if ( first ) first = false
            else push(",")
            e.output(this)
        } )
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
trait AnyRefType extends AnyTypeType

trait AccessModifierTag extends SyntaxGenerator
case class PrivateAccessTag extends AccessModifierTag { def output( s : SyntaxSink ) { s.push( "private" ) } }
case class ProtectedAccessTag extends AccessModifierTag { def output( s : SyntaxSink ) { s.push( "protected" ) } }
case class PublicAccessTag extends AccessModifierTag { def output( s : SyntaxSink ) { s.push( "public" ) } }


case class TupleType( val elements : List[ExprType] ) extends AnyRefType
{
    def output( s : SyntaxSink )
    {
        s.push("(")
        s.commaSeparated(elements)
        s.push(")")
    }
}

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
case class ConstantExpr( val value : Int ) extends ExprType
{
    def output( s : SyntaxSink )
    {
        s.push( value.toString )
    }
}

case class BinOpExpr( val lhs : ExprType, val rhs : ExprType, val fnName : String ) extends ExprType
{
    def output( s : SyntaxSink )
    {
        lhs.output(s)
        s.push( fnName )
        rhs.output(s)
    }
}

// Should really be an apply to deal properly with partial application
//case class FnCallExpr( val fn : ExprType, val args : List[ExprType] ) extends ExprType
//{
//}

case class BlockExpr( val contents : List[ExprType] ) extends ExprType
{
    def output( s : SyntaxSink )
    {
        s.inScope
        {
            contents.foreach(
            {
                c => c.output(s)
                s.eol()
            } )
        }
    }
}

case class IdentExpr( val id : String ) extends ExprType
{
    def output( s : SyntaxSink )
    {
        s.push(id)
    }
}

case class NewExpr( val toCreate : String, val args : List[ExprType] ) extends ExprType
{
    def output( s : SyntaxSink )
    {
        s.push( "new" )
        s.push( toCreate )
        s.push("(")
        s.commaSeparated(args)
        s.push(")")
    }
}

case class MethodCallExpr( val theObj : ExprType, val methName : String, val args : List[ExprType] ) extends ExprType
{
    def output( s : SyntaxSink )
    {
        theObj.output(s)
        s.push(".")
        s.push( methName )
        s.push("(")
        s.commaSeparated(args)
        s.push(")")
    }
}


// TODO: Fold in member function definitions
abstract class EntityDefinition extends ExprType

case class ValueDefinition( val name : String, val modifier : ValueModifierTag, val vType : Option[AnyTypeType], val expression : Option[ExprType], isLazy : Boolean ) extends EntityDefinition
{
    def output( s : SyntaxSink )
    {
        // val blah : Double = 12.0
        if ( isLazy ) s.push( "lazy" )
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
    }
}

case class MethodDefinition( val name : String, val accessModifier : AccessModifierTag, val params : List[ValueDefinition], val retType : Option[ExprType], val body : Some[ExprType] ) extends EntityDefinition
{
    def output( s : SyntaxSink )
    {
        //def blah( a : Int, b : Double ) = {}
        accessModifier.output(s)
        s.push( "def" )
        s.push( name )
        s.push("(")
        s.commaSeparated(params)
        s.push(")")
        retType.foreach( t =>
        {
            s.push(":")
            t.output(s)
        } )
        s.push("=")
        body.foreach( _.output(s) )
        s.eol()
    }
}

case class ConcreteTypeDefinition( val concreteType : ConcreteType ) extends EntityDefinition
{
    def output( s : SyntaxSink )
    {
        concreteType.output(s)
    }
}

// TODO: type parameters (generics with all the sorts of type constraints etc.)
class ConcreteType(
    val name : Option[String],
    val accessModifier : AccessModifierTag,
    val modifier : TypeModifierTag,
    val kind : TypeKindTag,
    val ctorParams : List[ValueDefinition],
    val superType : Option[String],
    val mixins : List[String] ) extends AnyRefType
{
    var definitions = mutable.ArrayBuffer[EntityDefinition]()
    def output( s : SyntaxSink )
    {
        accessModifier.output(s)
        modifier.output(s)
        kind.output(s)
        name.foreach( n => s.push(n) )
        
        s.push("(")
        var first = true
        ctorParams.foreach( p => {
            if ( first ) first = false
            else s.push(",")

            p.output(s)
        } )
        s.push(")")
        
        superType.foreach( t =>
        {
            s.push("extends")
            s.push(t)
        } )
        
        mixins.foreach( t =>
        {
            s.push("with")
            s.push(t)
        } )
        
        s inScope
        {
            definitions.foreach( v => v.output(s) )
        }
    }
}

class TypeGenerator( val name : String, val superType : Option[String], val mixins : List[String] )
{
    def exhaustive =
    {
        val accessModifiers = Array( new PrivateAccessTag(), new PublicAccessTag(), new ProtectedAccessTag() )
        val modifiers = Array(
            new TypeModifierTag( false, false ),
            new TypeModifierTag( false, true ),
            new TypeModifierTag( true, false ),
            new TypeModifierTag( false, false ) )
        val kinds = Array( new TraitKindTag(), new ObjectKindTag(), new ClassKindTag(), new AbstractClassKindTag() )
        
        for ( am <- accessModifiers; m <- modifiers; k <- kinds ) yield new ConcreteType( Some(name), am, m, k, List(), superType, mixins )
    }
}

object Main extends scala.App
{
    
/*
   Initial focus:
   
   * Lazy vals
   * By-name parameters
   * Closures
   * Anything defined in a method
   * Anything defined in an anonymous class
   * Anything with default arguments
   * Anything which uses the pattern matcher
   * Anything with structural types.
*/

    def test()
    {
        // Anonymous classes
        trait X
        trait Y
        
        def a = new
        {
            def dump( v : String )
            {
                print(v)
            }
        }
        
        a.dump("ook")
        
        def b = new X with Y
        {
            def dump( v : String )
            {
                println(v)
            }
        }
        
        // By name parameters
        def fn2( f : => Int )
        {
            val b : Int = f
        }
        
        // Structural types
        def call( a : Int, b : Int, obj : { def apply( x : Int, y : Int ) : Int } ) =
        {
            obj.apply( a, b )
        }
    }

    override def main(args : Array[String])
    {
        // A class and exhaustive inheritance
        {
            val s = new SyntaxSink()
            val ct = new ConcreteType( Some("Blah"), new PrivateAccessTag(), new TypeModifierTag( isFinal=true, isSealed=false ), new ClassKindTag(), List(), None, List() )
            // ValueDefinition( val name : String, val modifier : ValueModifierTag, val vType : Option[AnyTypeType], val expression : Option[ExprType] )
            ct.definitions.append( new ValueDefinition( "a", new VarValueTag(), Some( new IntType() ), None, false ) )
            ct.definitions.append( new ValueDefinition( "b", new VarValueTag(), Some( new IntType() ), Some( new ConstantExpr(3) ), false ) )
            ct.definitions.append( new ValueDefinition( "c", new ValValueTag(), None, Some( new ConstantExpr(5) ), false ) )
            ct.output(s)
            
            val tg = new TypeGenerator( "Foo", Some("Blah"), List() )
            for ( e <- tg.exhaustive )
            {
                e.output(s)
            }
        }
        
        // A hierarchy with a defined function
        {
            val s = new SyntaxSink()
            val ct = new ConcreteType( Some("Base"), new PublicAccessTag(), new TypeModifierTag( false, false ), new AbstractClassKindTag(), List(), None, List() )
            
            val ctd1 = new ConcreteType( Some("D1"), new PublicAccessTag(), new TypeModifierTag( false, false ), new AbstractClassKindTag(), List(), Some("Base"), List() )
            
            val ctd2 = new ConcreteType( Some("D2"), new PublicAccessTag(), new TypeModifierTag( false, false ), new ClassKindTag(), List(), Some("D1"), List() )
            
            val innerClass = new ConcreteType( Some("Inner"), new PublicAccessTag(), new TypeModifierTag( true, true ), new ClassKindTag(), List( new ValueDefinition( "increment", new NoneValueTag(), Some( new IntType() ), None, false ) ), None, List() )
            innerClass.definitions.append( new MethodDefinition( "apply", new PublicAccessTag(), List(
                new ValueDefinition( "x", new NoneValueTag(), Some( new IntType() ), None, false ) ),
                None,
                Some( new BinOpExpr( new IdentExpr( "increment" ), new IdentExpr( "x" ), "+" ) ) ) )
            
            ct.definitions.append( new MethodDefinition( "fn", new PublicAccessTag(), List(
                new ValueDefinition( "a", new NoneValueTag(), Some( new IntType() ), None, false ),
                new ValueDefinition( "b", new NoneValueTag(), Some( new IntType() ), None, false ),
                new ValueDefinition( "c", new NoneValueTag(), Some( new IntType() ), None, false ) ),
                None,
                Some( new BlockExpr( List(
                    new ConcreteTypeDefinition( innerClass ),
                    new ValueDefinition( "op", new ValValueTag(), None, Some(
                        new NewExpr( "Inner", List( new IdentExpr("a") ) ) ), false ),
 
                    new ValueDefinition( "res", new ValValueTag(), None, Some(
                        new MethodCallExpr( new IdentExpr("op"), "apply", List( new IdentExpr("b") ) ) ), true ),

                    new BinOpExpr( new IdentExpr( "res" ), new IdentExpr("c"), "*" )
                ) ) ) ) )
            
            ct.output(s)
            ctd1.output(s)
            ctd2.output(s)
        }
    }
}


