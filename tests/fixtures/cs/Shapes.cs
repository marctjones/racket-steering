using System;
using System.Collections.Generic;

namespace Demo.Shapes
{
    [Serializable]
    public class Guard
    {
        public static Guard Against { get; } = new Guard();

        private readonly int _limit = 3;
        public const string Name = "guard";

        public Guard() { }

        public Guard(int limit)
        {
            _limit = limit;
        }

        /// <summary>Checks for null.</summary>
        public static T Null<T>(T input, string parameterName)
        {
            if (input is null)
            {
                throw new ArgumentNullException(parameterName);
            }

            return input;
        }

        [Obsolete("use Null")]
        public static T Null<T>(
            T input,
            string parameterName,
            string? message)
        {
            return input ?? throw new ArgumentNullException(parameterName, message);
        }

        public int Limit => _limit;

        public string Describe(int x) => $"limit={_limit}, x={x}";

        public string Describe(string text)
        {
            var verbatim = @"C:\path\{not-a-brace}";
            var raw = """
                { "json": "value" }
                """;
            char open = '{';
            char quote = '"';
            // } a comment brace
            /* and { another */
            return $"{text}{{literal}}{verbatim.Length}";
        }

        public class Nested
        {
            public void Run() { }
        }

        #region helpers { unbalanced in a directive
        private static void Helper()
        {
        }
        #endregion
    }

    public partial class GuardExtensions
    {
        public static void First(this Guard g) { }
    }

    public partial class GuardExtensions
    {
        public static void Second(this Guard g) { }
    }

    public interface IGuardClause
    {
        void Check(int value);
        string Name { get; }
    }

    public enum Level
    {
        Low,
        High,
    }

    public record Point(int X, int Y)
    {
        public int Sum() => X + Y;
    }

    public struct Pair<TA, TB> where TA : class
    {
        public TA First;
        public TB Second;
    }
}
