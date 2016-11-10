package edu.richmond.nlp;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.AnnotationOutputter;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;

public final class AnnotationProcessor {

  TimeZone tz = TimeZone.getTimeZone("UTC");
  DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm'Z'");
  String output_path = "";
  String language = "";
  int idOffset = 0;

  public AnnotationProcessor() {}

  public void setOutputPath(String output_path) {
    this.output_path = output_path;
  }

  public void setLanguage(String language) {
    this.language = language;
  }

  public void setIdOffset(int idOffset) {
    this.idOffset = idOffset;
  }

  public void processFiles(String[] file_list, StanfordCoreNLP scnlp) throws FileNotFoundException, IOException {
    processFiles(Arrays.asList(file_list), scnlp);
  }

  public void processFiles(List<String> file_list, StanfordCoreNLP scnlp) throws FileNotFoundException, IOException {
    String corenlp_version = scnlp.getClass().getPackage().getImplementationVersion();
    df.setTimeZone(tz);
    boolean append = false;
    int docID = idOffset;

    for (int i = 0; i < file_list.size(); i++) {
      // record current time and start processing files
      String docIDString = Integer.toString(docID);
      String uri = file_list.get(i);
      String starttime = df.format(new Date());
      Annotation annotation = scnlp.process(readFileAsString(uri));

      // set up output options
      OutputStream output = null;
      AnnotationOutputter.Options opt = new AnnotationOutputter.Options();

      // save the annotation tables as CSV files
      CSVTokenOutputter tokenOut = new CSVTokenOutputter(docIDString, append);
      tokenOut.print(annotation, new FileOutputStream(output_path + "token.csv", append), opt);

      CSVDependencyOutputter depOut = new CSVDependencyOutputter(docIDString, append);
      depOut.print(annotation, new FileOutputStream(output_path + "dependency.csv", append), opt);

      CSVNamedEntityOutputter entityOut = new CSVNamedEntityOutputter(docIDString, append);
      entityOut.print(annotation, new FileOutputStream(output_path + "entity.csv", append), opt);

      CSVTripleOutputter tripleOut = new CSVTripleOutputter(docIDString, append);
      tripleOut.print(annotation, new FileOutputStream(output_path + "triple.csv", append), opt);

      CSVCoreferenceOutputter corefOut = new CSVCoreferenceOutputter(docIDString, append);
      corefOut.print(annotation, new FileOutputStream(output_path + "coreference.csv", append), opt);

      CSVSentimentOutputter sentimentOut = new CSVSentimentOutputter(docIDString, append);
      sentimentOut.print(annotation, new FileOutputStream(output_path + "sentiment.csv", append), opt);

      CSVDocumentOutputter docOut = new CSVDocumentOutputter(docIDString, append, language, starttime, uri, corenlp_version);
      docOut.print(annotation, new FileOutputStream(output_path + "document.csv", append), opt);

      append = true; // append on all but the first document
      docID++;
    }
  }

  public String readFileAsString(String file_name) throws FileNotFoundException, IOException, UnsupportedEncodingException {
    String s = "";
    File file = new File(file_name);
    FileInputStream fis = new FileInputStream(file);
    byte[] data = new byte[(int) file.length()];
    fis.read(data);
    fis.close();

    s = new String(data, "UTF-8");
    return s;
  }

}
