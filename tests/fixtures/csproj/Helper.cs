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
}
