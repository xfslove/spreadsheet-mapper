package me.excel.tools.transfer;

import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.model.excel.ExcelWorkbook;
import org.testng.annotations.Test;

import java.io.InputStream;
import java.util.stream.Collectors;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 15-12-20.
 */
public class ExcelFileTransferImplTest {

  @Test
  public void testTransfer() throws Exception {
    InputStream excelIs = this.getClass().getResourceAsStream("test.xlsx");

    ExcelFileTransferImpl transfer = new ExcelFileTransferImpl();
    transfer.transfer(excelIs);

    ExcelWorkbook excelWorkbook = transfer.getExcelWorkbook();

    assertEquals(excelWorkbook.sizeOfSheets(), 1);
    ExcelSheet sheet = excelWorkbook.getSheet(0);
    assertEquals(sheet.getSheetName(), "Sheet0");

    assertEquals(sheet.sizeOfRows(), 2);
    for (int i = 0; i < 2; i++) {
      ExcelRow row = sheet.getRow(i);
      assertEquals(row.sizeOfCells(), 4);
      if (i == 0) {
        assertEquals(row.getCells().stream()
            .map(cell -> cell.getValue()).collect(Collectors.toList()).toString(), "[111111, std1, 18, 2015-09-01]");
      } else {
        assertEquals(row.getCells().stream()
            .map(cell -> cell.getValue()).collect(Collectors.toList()).toString(), "[2222, std2, 18, 2015-09-01]");
      }
    }

  }
}