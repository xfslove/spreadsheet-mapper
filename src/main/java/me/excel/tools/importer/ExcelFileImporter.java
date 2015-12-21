package me.excel.tools.importer;

import me.excel.tools.processor.DataProcessor;
import me.excel.tools.transfer.ExcelFileTransfer;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

/**
 * excel 文件导入器
 *
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileImporter extends ExcelFileTransfer implements UserFileImporter {

  @Override
  public void process(File file, DataProcessor dataProcessor) throws IOException {

    if (file == null) {
      throw new IllegalArgumentException("file is null");
    }
    if (dataProcessor == null) {
      throw new IllegalArgumentException("dataProcessor is null");
    }

    FileInputStream inputStream = new FileInputStream(file);

    transfer(inputStream);

    // 交给processor
    excelWorkbook.getSheets().forEach(excelSheet -> dataProcessor.handle(excelSheet));
  }

}
