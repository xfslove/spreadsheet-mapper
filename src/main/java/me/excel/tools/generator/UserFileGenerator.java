package me.excel.tools.generator;

import me.excel.tools.extractor.FieldValueExtractor;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

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
   * @param fieldValueExtractors field value extractor
   * @see FieldValueExtractor
   */
  void addValueExtractors(FieldValueExtractor... fieldValueExtractors);

  /**
   * @param file intend write file
   * @throws IOException io exception
   * @see #generate(File, SheetContext...)
   */
  void generate(File file, SheetContext... contexts) throws IOException;

  /**
   * generate file to supplied output stream
   *
   * @param outputStream intend write stream, notice close
   * @throws IOException io exception
   */
  void generate(OutputStream outputStream, SheetContext... contexts) throws IOException;
}
