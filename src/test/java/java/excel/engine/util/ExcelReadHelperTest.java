package java.excel.engine.util;

import java.excel.engine.ExcelConstants;
import java.excel.engine.model.excel.Cell;
import java.excel.engine.model.excel.Row;
import java.excel.engine.model.excel.Sheet;
import java.excel.engine.model.excel.Workbook;
import org.testng.annotations.Test;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 15-12-20.
 */
public class ExcelReadHelperTest {

  @Test
  public void testTransfer() throws Exception {

    InputStream excelIs = this.getClass().getResourceAsStream("test" + ExcelConstants.SUFFIX_XLSX);

    ExcelDateFormatRegister.registerFormat("[$-409]d\\-mmm\\-yy;@", "yyyy-MM-dd");

    Workbook workbook = ExcelReadHelper.read(excelIs);

    assertEquals(workbook.sizeOfSheets(), 1);
    Sheet sheet = workbook.getSheet(1);
    assertEquals(sheet.getName(), "Sheet0");

    assertEquals(sheet.sizeOfRows(), 2);
    for (int i = 0; i < 2; i++) {
      Row row = sheet.getRow(i + 1);
      assertEquals(row.sizeOfCells(), 4);
      if (i == 0) {
        List<String> values = new ArrayList<>();
        for (Cell cell : row.getCells()) {
          values.add(cell.getValue());
        }
        assertEquals(values.toString(), "[111111, std1, 18, 2015-10-01]");
      } else {
        List<String> values = new ArrayList<>();
        for (Cell cell : row.getCells()) {
          values.add(cell.getValue());
        }
        assertEquals(values.toString(), "[2222, std2, 18, 2015-10-01]");
      }
    }

  }
}