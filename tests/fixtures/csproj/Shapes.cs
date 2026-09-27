using System;

namespace App
{
    // T63 fixture: exercised by the C# extractor's conformance test. Covers a same-file exact call,
    // a project-wide name-match call (Helper.Double - no dotnet SDK means no reliable using->file
    // resolution), a class hierarchy with an override reached via `base.` and `this.`, `new T()` as
    // a constructor call, an attribute, and a helper reachable from nothing (so a later reachability
    // pass has something real to call dead).

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

        public static int UnusedHelper(int x)
        {
            return Helper.Triple(x);
        }
    }
}
