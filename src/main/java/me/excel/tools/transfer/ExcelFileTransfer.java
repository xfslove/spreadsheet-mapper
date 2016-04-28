package me.excel.tools.transfer;

import me.excel.tools.model.excel.ExcelWorkbook;

import java.io.InputStream;

/**
 * excel transfer
 *
 * Created by hanwen on 15-12-16.
 */
public interface ExcelFileTransfer {

  /**
   * transfer inputStream to java bean
   *
   * @param inputStream
   */
  ExcelWorkbook transfer(InputStream inputStream);

}
