import scala.collection.{mutable, immutable}

/*
   
  Paul P Notes:
   
 - many combinations would be invalid scala.  This is fine - how the compiler handles invalid code is no less important.
 - clearly it wouldn't be hard to take n^n far enough that exhaustion becomes impossible.  That's fine too; the next step would be to have a filtering mechanism so one can use a predicate to reduce the space to more interesting cases.
 -  you could get a long and super useful already ways just doing something like I did in the primitive version, smooshing strings together.  But it gets too hard to test anything nontrivial, like "this type parameter has that type parameter as one of its bounds and then..." but many of our bugs require things to refer to one another like that.
 - one could consider doing this with real scala AST (DefDef, ClassDef, etc) rather than generating source from scratch.  I don't like that idea because I think it falls to "regulatory capture" - by assuming the language of the compiler, one becomes blind to whole categories of bugs.

*/

class TypeGenerator( val name : String, val superType : Option[String], val mixins : List[String] )
{
    def exhaustive =
    {
        val accessModifiers = Array( new PrivateAccessTag(), new PublicAccessTag(), new ProtectedAccessTag() )
        val modifiers = Array(
            new TypeModifierTag( isFinal=false, isSealed=false ),
            new TypeModifierTag( isFinal=false, isSealed=true ),
            new TypeModifierTag( isFinal=true, isSealed=false ),
            new TypeModifierTag( isFinal=false, isSealed=false ) )
            
        val kinds = Array( new TraitKindTag(), new ObjectKindTag(), new ClassKindTag(), new AbstractClassKindTag() )
        
        for ( am <- accessModifiers; m <- modifiers; k <- kinds ) yield new ConcreteType(
            name=Some(name),
            accessModifier=am,
            modifier=m,
            kind=k,
            ctorParams=List(),
            superType=superType,
            mixins=mixins )
    }
}


object Main extends scala.App
{
    
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

    def singleInheritanceOverrideCombinations()
    {
        val s = new SyntaxSink()
        val tg = new TypeGenerator(name="Base", superType=None, mixins=List() )
        
        for ( bat <- List[AccessModifierTag]( new PrivateAccessTag(), new PublicAccessTag(), new ProtectedAccessTag() ) )
        {
            for ( dat <- List[AccessModifierTag]( new PrivateAccessTag(), new PublicAccessTag(), new ProtectedAccessTag() ) )
            {
                // Override with val, lazy val, def
                val overrides = List(
                    new ValueDefinition(
                        name="fn",
                        accessModifier=dat,
                        modifier= new ValValueTag(),
                        vType=Some( new IntType() ),
                        expression=Some( new ConstantExpr( 3 ) ),
                        isLazy=false ),
                    new ValueDefinition(
                        name="fn",
                        accessModifier=dat,
                        modifier= new ValValueTag(),
                        vType=Some( new IntType() ),
                        expression = Some( new ConstantExpr( 3 ) ),
                        isLazy=false ),
                    new MethodDefinition(
                        name="fn",
                        accessModifier=dat,
                        params=List(),
                        retType=Some( new IntType() ),
                        body=Some( new ConstantExpr(3) ) ) )
                        
                for ( ot <- overrides )
                {
                    for ( b <- tg.exhaustive )
                    {
                        // Function to be overriden
                        b.definitions.append( new MethodDefinition(
                            name="fn",
                            accessModifier=bat,
                            params=List(),
                            retType=Some( new IntType() ),
                            body=None) )
                        
                        for ( d <- (new TypeGenerator("Derived", superType=Some("Base"), mixins=List() ) ).exhaustive )
                        {
                            d.definitions.append( ot )
                            
                            b.output(s)
                            d.output(s)
                        }
                    }
                }
            }
        }
    }
    
    def experimenting()
    {
        val s = new SyntaxSink()
        val ct = new ConcreteType( Some("Base"), new PublicAccessTag(), new TypeModifierTag( false, false ), new AbstractClassKindTag(), List(), None, List() )
        
        val ctd1 = new ConcreteType( Some("D1"), new PublicAccessTag(), new TypeModifierTag( false, false ), new AbstractClassKindTag(), List(), Some("Base"), List() )
        
        val ctd2 = new ConcreteType( Some("D2"), new PublicAccessTag(), new TypeModifierTag( false, false ), new ClassKindTag(), List(), Some("D1"), List() )
        
        val innerClass = new ConcreteType( Some("Inner"), new PublicAccessTag(), new TypeModifierTag( true, true ), new ClassKindTag(), List( new ValueDefinition( "increment", new PublicAccessTag(), new NoneValueTag(), Some( new IntType() ), None, false ) ), None, List() )
        innerClass.definitions.append( new MethodDefinition( "apply", new PublicAccessTag(), List(
            new ValueDefinition( "x", new PublicAccessTag(), new NoneValueTag(), Some( new IntType() ), None, false ) ),
            None,
            Some( new BinOpExpr( new IdentExpr( "increment" ), new IdentExpr( "x" ), "+" ) ) ) )
        
        ct.definitions.append( new MethodDefinition( "fn", new PublicAccessTag(), List(
            new ValueDefinition( "a", new PublicAccessTag(), new NoneValueTag(), Some( new IntType() ), None, false ),
            new ValueDefinition( "b", new PublicAccessTag(), new NoneValueTag(), Some( new IntType() ), None, false ),
            new ValueDefinition( "c", new PublicAccessTag(), new NoneValueTag(), Some( new IntType() ), None, false ) ),
            None,
            Some( new BlockExpr( List(
                new ConcreteTypeDefinition( innerClass ),
                new ValueDefinition( "op", new PublicAccessTag(), new ValValueTag(), None, Some(
                    new NewExpr( "Inner", List( new IdentExpr("a") ) ) ), false ),

                new ValueDefinition( "res", new PublicAccessTag(), new ValValueTag(), None, Some(
                    new MethodCallExpr( new IdentExpr("op"), "apply", List( new IdentExpr("b") ) ) ), true ),

                new BinOpExpr( new IdentExpr( "res" ), new IdentExpr("c"), "*" )
            ) ) ) ) )
        
        ct.output(s)
        ctd1.output(s)
        ctd2.output(s)
    }
  
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

    override def main(args : Array[String])
    {
        experimenting()
        singleInheritanceOverrideCombinations()
    }
}


