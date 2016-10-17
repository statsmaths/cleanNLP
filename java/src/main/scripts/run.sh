export CLASSPATH=target/cleanNLP-0.1.jar:/Users/taylor/Documents/stanford-corenlp-full-2015-12-09/*
export JAVA_HEAP_MAX="-Xmx8192m"

java $JAVA_HEAP_MAX -cp $CLASSPATH edu.richmond.nlp.RunClient
