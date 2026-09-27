namespace App
{
    // Not imported by path (no dotnet SDK / no namespace index available to resolve-import - see
    // cs-extract.rkt's module comment): calls into this file resolve via the graph's project-wide
    // name-match fallback, exercising that C# gets a lower "declared" ratio than Racket/Python.
    public static class Helper
    {
        public static int Double(int x)
        {
            return x * 2;
        }

        public static int Triple(int x)
        {
            return x * 3;
        }
    }

    // T65/T70 follow-up (notes/16 SS8/SS9): a property BARE-named "Attribute" - the exact shape of
    // ardalis/GuardClauses' real AspRequiredAttributeAttribute.Attribute, which used to let ANY
    // `: Attribute` base with no in-project Attribute TYPE (Shapes.cs's LoudAttribute etc. - their
    // real base, System.Attribute, is a BCL type this graph never sees) fall back to the project-wide
    // bare-name-match and land here instead of going external, purely because a base-class reference
    // was matched against a property, not a type.
    public class AttributeHolder
    {
        public string Attribute { get; }
    }
}
