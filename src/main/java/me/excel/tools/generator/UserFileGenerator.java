package me.excel.tools.generator;

import me.excel.tools.extractor.FieldValueExtractor;
import me.excel.tools.prompter.FieldPrompter;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

/**
 * <pre>
 * file generator, generated all cell type is string (include number, date ...).
 *
 * file format see {@link me.excel.tools.factory.UserFileTemplate}
 * </pre>
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileGenerator {

  /**
   * set titles (first row)
   *
   * @param titles
   */
  void setTitles(String... titles);

  /**
   * set fields (second row)
   *
   * @param fields
   */
  void setFields(String... fields);

  /**
   * set data to generate data row (after third row)
   *
   * @param data
   */
  void setData(List data);

  /**
   * @param fieldValueExtractors
   * @see FieldValueExtractor
   */
  void addValueExtractors(FieldValueExtractor... fieldValueExtractors);

  /**
   * @param fieldPrompters
   * @see FieldPrompter
   */
  void addCellPrompters(FieldPrompter... fieldPrompters);

  /**
   * @param file
   * @throws IOException
   * @see #generate(OutputStream)
   */
  void generate(File file) throws IOException;

  /**
   * generate file to supplied output stream
   *
   * @throws IOException
   */
  void generate(OutputStream outputStream) throws IOException;

  /**
   * @param file
   * @param createTitles
   * @param createFields
   * @param createPrompts
   * @throws IOException
   * @see #generate(OutputStream, boolean, boolean, boolean)
   */
  void generate(File file, boolean createTitles, boolean createFields, boolean createPrompts) throws IOException;

  /**
   * @param outputStream
   * @param createTitles  detect if show titles
   * @param createFields  detect if show fields
   * @param createPrompts detect if show prompts
   * @throws IOException
   * @see #generate(OutputStream)
   */
  void generate(OutputStream outputStream, boolean createTitles, boolean createFields, boolean createPrompts) throws IOException;
}
