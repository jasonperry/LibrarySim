using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BuildCat
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Welcome to the catalog builder.");
            Console.WriteLine(
                "Specify either a set of MarcXML files, \n" +
                "or a single RDF Database, \n" +
                "and indicate whether to create a new database \n" +
                "or append to an existing one.");
        }
    }
}
