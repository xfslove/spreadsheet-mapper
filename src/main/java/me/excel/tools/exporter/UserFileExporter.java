package me.excel.tools.exporter;

import java.io.IOException;
import java.io.OutputStream;

/**
 * file exporter
 *
 * Created by hanwen on 15-12-16.
 */
public interface UserFileExporter {

  /**
   * export java bean to outputStream
   *
   * @param outputStream
   * @throws IOException
   */
  void export(OutputStream outputStream) throws IOException;
}
