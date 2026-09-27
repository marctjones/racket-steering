using System;

namespace App
{
    // T63 fixture: exercised by the C# extractor's conformance test. Covers a same-file exact call,
    // a project-wide name-match call (Helper.Double - no dotnet SDK means no reliable using->file
    // resolution), a class hierarchy with an override reached via `base.` and `this.`, `new T()` as
    // a constructor call, an attribute, and a helper reachable from nothing (so a later reachability
    // pass has something real to call dead).

    // T63 follow-up (notes/16 SS3): two project-defined attribute classes, used the way real C# code
    // almost always writes them - the short form, without the class's own "Attribute" suffix - so the
    // extractor's dual-emission (bare name + suffixed name) is exercised for real, not just on a BCL
    // attribute like [Serializable] (which never resolves to a project symbol either way).
    public class LoudAttribute : Attribute { }
    public class QuietAttribute : Attribute { }

    [Serializable]
    public abstract class Animal
    {
        public abstract string Speak();
    }

    public class Dog : Animal
    {
        public override string Speak()
        {
            return this.Bark() + Helper.Double(1).ToString();
        }

        [Loud]
        public string Bark()
        {
            return "woof";
        }
    }

    public class LoudDog : Dog
    {
        public override string Speak()
        {
            return base.Speak() + "!!!";
        }
    }

    public class Program
    {
        // steer: entry
        public static void Run()
        {
            var d = new Dog();
            Console.WriteLine(d.Speak());
        }

        [Quiet]
        public static int UnusedHelper(int x)
        {
            return Helper.Triple(x);
        }
    }
}
