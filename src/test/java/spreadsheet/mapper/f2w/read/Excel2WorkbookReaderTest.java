package spreadsheet.mapper.f2w.read;

import org.testng.annotations.Test;
import spreadsheet.mapper.AssertUtil;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.core.Workbook;
import spreadsheet.mapper.f2w.DateFormatRegister;

import java.io.InputStream;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2017/1/5.
 */
@Test(groups = "Excel2WorkbookReaderTest")
public class Excel2WorkbookReaderTest {

  @Test
  public void testRead() throws Exception {

    DateFormatRegister.GLOBAL.register("[$-409]d\\-mmm\\-yy;@", "yyyy-MM-dd");
    DateFormatRegister.GLOBAL.register("mm/dd/yy;@", "yyyy-MM-dd");
    DateFormatRegister.GLOBAL.register("m/d/yy", "yyyy-MM-dd");

    InputStream is1 = getClass().getResourceAsStream("test.xls");

    WorkbookReader reader = new Excel2WorkbookReader();

    Workbook workbook1 = reader.read(is1);

    AssertUtil.assertWorkbookEquals(workbook1, true);

    InputStream is2 = getClass().getResourceAsStream("test.xlsx");

    Workbook workbook2 = reader.read(is2);

    AssertUtil.assertWorkbookEquals(workbook2, true);
  }

  @Test(dependsOnMethods = "testRead")
  public void testReadDate() throws Exception {

    InputStream is1 = getClass().getResourceAsStream("dateFormatTest.xlsx");

    WorkbookReader reader = new Excel2WorkbookReader();

    Workbook workbook = reader.read(is1);

    Sheet firstSheet = workbook.getFirstSheet();

    Row firstRow = firstSheet.getFirstRow();

    assertEquals(firstRow.getCell(1).getValue(), "1984-11-22");
    assertEquals(firstRow.getCell(2).getValue(), "1984-11-22");
    assertEquals(firstRow.getCell(3).getValue(), "1984-11-22");
    assertEquals(firstRow.getCell(4).getValue(), "1984-11-22");
  }
}