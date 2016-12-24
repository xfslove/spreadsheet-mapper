package me.excel.tools.exporter;

import java.io.IOException;
import java.io.OutputStream;

/**
 * file exporter
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileExporter {

  /**
   * export file to supplied output stream
   *
   * @param outputStream notice close the stream
   * @throws IOException io exception
   */
  void export(OutputStream outputStream) throws IOException;
}
