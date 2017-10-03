using System;
using System.IO;
using System.Xml.Serialization;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BuildCat
{
    class Program
    {
        static bool ProcessMarcXMLFile(string filePath)
        {
            Console.WriteLine("Processing xml file " + filePath);
            XmlSerializer ser = new XmlSerializer(typeof(recordType));
            recordType record = 
                ser.Deserialize(new FileStream(filePath, FileMode.Open)) 
                    as recordType;

            if (record != null) 
            {
                // do stuff();
                return true;
            }
            Console.WriteLine("Error processing MarcXML file " + filePath);
            return false;
        }

        static void Main(string[] args)
        {
            if (args.Length != 2)
            {
                // COMMAND-LINE ARGUMENTS: folder of MarcXMLs, CN type
                Console.WriteLine("catalog builder usage.");
                Console.WriteLine(
                    "buildcat.exe <MarcXML folder> <--loc|--dewey|--alpha>");
                return; // exit program?
            }
            var scanPath = args[0];
            var cnType = args[1];
            Console.WriteLine(scanPath + ", " + cnType);
            // NOTE: can't follow windows .lnk files
            if (Directory.Exists(scanPath))
            {
                Console.WriteLine("yah!");
                string[] fileEntries = Directory.GetFiles(scanPath);
                int filesFound = 0;
                int filesSuccess = 0;
                foreach (string fileName in fileEntries) {
                    if (fileName.EndsWith(".xml")) {
                        filesFound++;
                        if (ProcessMarcXMLFile(fileName))//Path.Combine(scanPath, fileName)))
                            filesSuccess++;
                    }
                }
                Console.WriteLine("Processed {0} files, {1} successful",
                                  filesFound, filesSuccess);
            }
            else
            {
                Console.WriteLine("Error: not a directory");
                return;
            }

        }
    }
}
