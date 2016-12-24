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
   * @param titles titles
   */
  void setTitles(String... titles);

  /**
   * set fields (second row)
   *
   * @param fields fields
   */
  void setFields(String... fields);

  /**
   * set data to generate data row (after third row)
   *
   * @param data model collection
   */
  void setData(List data);

  /**
   * @param fieldValueExtractors field value extractor
   * @see FieldValueExtractor
   */
  void addValueExtractors(FieldValueExtractor... fieldValueExtractors);

  /**
   * @param fieldPrompters field prompter
   * @see FieldPrompter
   */
  void addCellPrompters(FieldPrompter... fieldPrompters);

  /**
   * @param file intend write file
   * @throws IOException io exception
   * @see #generate(OutputStream)
   */
  void generate(File file) throws IOException;

  /**
   * generate file to supplied output stream
   *
   * @param outputStream intend write stream, notice close
   * @throws IOException io exception
   */
  void generate(OutputStream outputStream) throws IOException;

  /**
   * @param file          intend write file
   * @param createTitles  detect if show titles
   * @param createFields  detect if show fields
   * @param createPrompts detect if show prompts
   * @throws IOException io exception
   * @see #generate(OutputStream, boolean, boolean, boolean)
   */
  void generate(File file, boolean createTitles, boolean createFields, boolean createPrompts) throws IOException;

  /**
   * @param outputStream  intend write stream, notice close
   * @param createTitles  detect if show titles
   * @param createFields  detect if show fields
   * @param createPrompts detect if show prompts
   * @throws IOException io exception
   * @see #generate(OutputStream)
   */
  void generate(OutputStream outputStream, boolean createTitles, boolean createFields, boolean createPrompts) throws IOException;
}
