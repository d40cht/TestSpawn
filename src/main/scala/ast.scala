import scala.collection.{mutable, immutable}

// Writer for outputing syntax to file
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
    
    def inScope( contents : => Unit )
    {
        eol()
        push("{")
        eol()
        indent += 1
        contents
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
class DoubleType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Double" ) } }
class FloatType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Float" ) } }
class LongType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Long" ) } }
class IntType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Int" ) } }
class ShortType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Short" ) } }
class ByteType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Byte" ) } }
class CharType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Char" ) } }
class BooleanType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Boolean" ) } }
class UnitType extends AnyValType { def output( s : SyntaxSink ) { s.push( "Unit" ) } }

// AnyRef
trait AnyRefType extends AnyTypeType

trait AccessModifierTag extends SyntaxGenerator
class PrivateAccessTag extends AccessModifierTag { def output( s : SyntaxSink ) { s.push( "private" ) } }
class ProtectedAccessTag extends AccessModifierTag { def output( s : SyntaxSink ) { s.push( "protected" ) } }
class PublicAccessTag extends AccessModifierTag { def output( s : SyntaxSink ) { s.push( "public" ) } }


class TupleType( val elements : List[ExprType] ) extends AnyRefType
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
class TraitKindTag extends TypeKindTag { def output( s : SyntaxSink ) { s.push( "trait" ) } }
class ObjectKindTag extends TypeKindTag { def output( s : SyntaxSink ) { s.push( "object" ) } }
class ClassKindTag extends TypeKindTag { def output( s : SyntaxSink ) { s.push( "class" ) } }
class AbstractClassKindTag extends TypeKindTag { def output( s : SyntaxSink ) { s.push( "abstract class" ) } }

trait ValueModifierTag extends SyntaxGenerator
class ValValueTag extends ValueModifierTag { def output( s : SyntaxSink ) { s.push( "val" ) } }
class VarValueTag extends ValueModifierTag { def output( s : SyntaxSink ) { s.push( "var" ) } }
class NoneValueTag extends ValueModifierTag { def output( s : SyntaxSink ) {} }

trait ExprType extends SyntaxGenerator
class ConstantExpr( val value : Int ) extends ExprType
{
    def output( s : SyntaxSink )
    {
        s.push( value.toString )
    }
}

class BinOpExpr( val lhs : ExprType, val rhs : ExprType, val fnName : String ) extends ExprType
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

class BlockExpr( val contents : List[ExprType] ) extends ExprType
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

class IdentExpr( val id : String ) extends ExprType
{
    def output( s : SyntaxSink )
    {
        s.push(id)
    }
}

class NewExpr( val toCreate : String, val args : List[ExprType] ) extends ExprType
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

class MethodCallExpr( val theObj : ExprType, val methName : String, val args : List[ExprType] ) extends ExprType
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

class ValueDefinition( val name : String, val accessModifier: AccessModifierTag, val modifier : ValueModifierTag, val vType : Option[AnyTypeType], val expression : Option[ExprType], isLazy : Boolean ) extends EntityDefinition
{
    def output( s : SyntaxSink )
    {
        // val blah : Double = 12.0
        accessModifier.output(s)
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

class MethodDefinition( val name : String, val accessModifier : AccessModifierTag, val params : List[ValueDefinition], val retType : Option[AnyTypeType], val body : Option[ExprType] ) extends EntityDefinition
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

        body.foreach( b =>
        {
            s.push("=")
            b.output(s)
        } )
    }
}

class ConcreteTypeDefinition( val concreteType : ConcreteType ) extends EntityDefinition
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
        s.commaSeparated( ctorParams )
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
            definitions.foreach( v =>
            {
                v.output(s)
                s.eol()
            } )
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

