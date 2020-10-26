using Microsoft.FSharp.Collections;
using polynomial_exercise;
using System;
using System.Linq;

namespace polynomial_usage_test
{
    class Program
    {
        static void Main(string[] args)
        {
            var p1 = polynomial.ofArray(new[] { 1, 2, 3, 4 });
            var p2 = polynomial.ofArray(new[] { 2, 3, 4, 5 });

            var added = polynomial.add(p1, p2);
            var multiplied = polynomial.mul(p1, p2);
            var p1Derivative = polynomial.derivative(p1);
            var degreeOfDerivavedP1 = polynomial.degToString(polynomial.deg(p1Derivative));
            
            Console.WriteLine(degreeOfDerivavedP1);
            Console.WriteLine(polynomial.toString(added));
            Console.WriteLine(polynomial.toString(multiplied));


        }
    }
}
