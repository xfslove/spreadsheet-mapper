package excel.engine.f2w;

import excel.engine.ExcelConstants;
import excel.engine.util.ExcelDateFormatRegister;
import excel.engine.model.core.Cell;
import excel.engine.model.core.Row;
import excel.engine.model.core.Sheet;
import excel.engine.model.core.Workbook;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 15-12-20.
 */
public class Excel2WorkbookReaderTest {

  @Test
  public void testTransfer() throws Exception {

    InputStream excelIs = this.getClass().getResourceAsStream("test" + ExcelConstants.SUFFIX_XLSX);

    ExcelDateFormatRegister.registerFormat("[$-409]d\\-mmm\\-yy;@", "yyyy-MM-dd");

    WorkbookReader reader = new Excel2WorkbookReader();
    Workbook workbook = reader.read(excelIs);

    Assert.assertEquals(workbook.sizeOfSheets(), 1);
    Sheet sheet = workbook.getSheet(1);
    Assert.assertEquals(sheet.getName(), "Sheet0");

    Assert.assertEquals(sheet.sizeOfRows(), 2);
    for (int i = 0; i < 2; i++) {
      Row row = sheet.getRow(i + 1);
      Assert.assertEquals(row.sizeOfCells(), 4);
      if (i == 0) {
        List<String> values = new ArrayList<>();
        for (Cell cell : row.getCells()) {
          values.add(cell.getValue());
        }
        Assert.assertEquals(values.toString(), "[111111, std1, 18, 2015-10-01]");
      } else {
        List<String> values = new ArrayList<>();
        for (Cell cell : row.getCells()) {
          values.add(cell.getValue());
        }
        Assert.assertEquals(values.toString(), "[2222, std2, 18, 2015-10-01]");
      }
    }

  }
}