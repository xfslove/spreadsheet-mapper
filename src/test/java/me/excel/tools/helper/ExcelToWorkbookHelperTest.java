package me.excel.tools.helper;

import me.excel.tools.ExcelConstants;
import me.excel.tools.ExcelSupportedDateFormat;
import me.excel.tools.helper.ExcelToWorkbookHelper;
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
public class ExcelToWorkbookHelperTest {

  @Test
  public void testTransfer() throws Exception {

    InputStream excelIs = this.getClass().getResourceAsStream("test" + ExcelConstants.SUFFIX_XLSX);

    ExcelSupportedDateFormat.registerFormat("[$-409]d\\-mmm\\-yy;@", "yyyy-MM-dd");

    ExcelWorkbook excelWorkbook = ExcelToWorkbookHelper.read(excelIs);

    assertEquals(excelWorkbook.sizeOfSheets(), 1);
    ExcelSheet sheet = excelWorkbook.getSheet(1);
    assertEquals(sheet.getSheetName(), "Sheet0");

    assertEquals(sheet.sizeOfRows(), 2);
    for (int i = 0; i < 2; i++) {
      ExcelRow row = sheet.getRow(i + 1);
      assertEquals(row.sizeOfCells(), 4);
      if (i == 0) {
        assertEquals(row.getCells().stream()
            .map(cell -> cell.getValue()).collect(Collectors.toList()).toString(), "[111111, std1, 18, 2015-10-01]");
      } else {
        assertEquals(row.getCells().stream()
            .map(cell -> cell.getValue()).collect(Collectors.toList()).toString(), "[2222, std2, 18, 2015-10-01]");
      }
    }

  }
}