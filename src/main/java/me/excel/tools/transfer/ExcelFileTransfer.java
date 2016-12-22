package me.excel.tools.transfer;

import me.excel.tools.model.excel.ExcelWorkbook;

import java.io.IOException;
import java.io.InputStream;

/**
 * excel transfer
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ExcelFileTransfer {

  /**
   * transfer supplied excel stream to {@link ExcelWorkbook}
   *
   * @param inputStream auto close
   */
  ExcelWorkbook transfer(InputStream inputStream) throws IOException;

}
