package edu.richmond.nlp;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.List;

import edu.stanford.nlp.coref.data.CorefChain;
import edu.stanford.nlp.io.IOUtils;
import edu.stanford.nlp.ling.*;
import edu.stanford.nlp.util.CoreMap;

import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.AnnotationOutputter;

public class CSVCoreferenceOutputter extends AnnotationOutputter {

  private CSVCoreferenceDocumentWriter csvWriter = new CSVCoreferenceDocumentWriter("0");
  private boolean append = false;

  public CSVCoreferenceOutputter(String docID, boolean append) {
    this.csvWriter.setDocID(docID);
    this.append = append;
  }

  @Override
  public void print(Annotation doc, OutputStream target, Options options) throws IOException {
    PrintWriter writer = new PrintWriter(IOUtils.encodedOutputStreamWriter(target, options.encoding));
    List<CoreMap> sentences = doc.get(CoreAnnotations.SentencesAnnotation.class);

    if (!append) {
      writer.print(csvWriter.header);
    }

    writer.print(csvWriter.print(doc));
    writer.flush();
  }

}

