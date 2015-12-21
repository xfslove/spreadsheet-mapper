package me.excel.tools.transfer;

import me.excel.tools.model.excel.ExcelWorkbook;
import org.apache.poi.ss.usermodel.Workbook;
import org.testng.annotations.Test;

import java.io.File;
import java.io.FileInputStream;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 15-12-20.
 */
public class ExcelFileTransferTest {
  
  @Test
  public void testTransfer() throws Exception {

    MyFileTransfer fileTransfer = new MyFileTransfer();

    File file = new File("/home/hanwen/tmp/test.xlsx");

    fileTransfer.transfer(new FileInputStream(file));

    ExcelWorkbook workbook = fileTransfer.getWorkbook();

  }

  private class MyFileTransfer extends ExcelFileTransfer {

    ExcelWorkbook getWorkbook() {
      return excelWorkbook;
    }
  }
}