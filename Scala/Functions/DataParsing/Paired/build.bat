scalac -sourcepath src -d bin -classpath /usr/lusers/wmchad/software/spark-0.8.0-incubating/examples/target/scala-2.9.3/spark-examples-assembly-0.8.0-incubating.jar PairedParser.scala 

cd bin
jar -cfm ../PairedParser.jar ../MANIFEST.MF *.*
cd ..

#java -jar PairedParser.jar
