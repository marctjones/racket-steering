namespace Demo.Scoped;

public class Outer
{
    public int Count { get; set; }

    public int this[int index] => index * Count;

    public event EventHandler? Changed;

    public static Outer operator +(Outer a, Outer b) => new Outer { Count = a.Count + b.Count };

    ~Outer() { }

    public class Inner
    {
        public string Value(string key) => key;
    }
}
