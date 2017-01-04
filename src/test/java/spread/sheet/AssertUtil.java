package spread.sheet;

import spread.sheet.model.core.*;

import java.util.Map;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2017/1/5.
 */
public class AssertUtil {

  private AssertUtil() {
    // default constructor
  }

  public static void assertWorkbookEquals(Workbook workbook) {

    assertEquals(workbook.sizeOfSheets(), 1);

    Sheet sheet = workbook.getSheet(1);

    assertEquals(sheet.sizeOfRows(), 3);

    Row r2 = sheet.getRow(2);
    Row r3 = sheet.getRow(3);

    Map<String, Cell> cellMap1 = TestFactory.createCellMap1();

    assertEquals(r2.getCell(1), cellMap1.get("test.int1"));
    assertEquals(r2.getCell(2), cellMap1.get("test.int2"));
    assertEquals(r2.getCell(3), cellMap1.get("test.long1"));
    assertEquals(r2.getCell(4), cellMap1.get("test.long2"));
    assertEquals(r2.getCell(5), cellMap1.get("test.float1"));
    assertEquals(r2.getCell(6), cellMap1.get("test.float2"));
    assertEquals(r2.getCell(7), cellMap1.get("test.double1"));
    assertEquals(r2.getCell(8), cellMap1.get("test.double2"));
    assertEquals(r2.getCell(9), cellMap1.get("test.boolean1"));
    assertEquals(r2.getCell(10), cellMap1.get("test.boolean2"));
    assertEquals(r2.getCell(11), cellMap1.get("test.string"));
    assertEquals(r2.getCell(12), cellMap1.get("test.bigDecimal"));
    assertEquals(r2.getCell(13), cellMap1.get("test.localDate"));
    assertEquals(r2.getCell(14), cellMap1.get("test.localDateTime"));

    Map<String, Cell> cellMap2 = TestFactory.createCellMap2();
    assertEquals(r3.getCell(1), cellMap2.get("test.int1"));
    assertEquals(r3.getCell(2), CellBean.EMPTY_CELL(2));
    assertEquals(r3.getCell(3), cellMap2.get("test.long1"));
    assertEquals(r3.getCell(4), CellBean.EMPTY_CELL(4));
    assertEquals(r3.getCell(5), cellMap2.get("test.float1"));
    assertEquals(r3.getCell(6), CellBean.EMPTY_CELL(6));
    assertEquals(r3.getCell(7), cellMap2.get("test.double1"));
    assertEquals(r3.getCell(8), CellBean.EMPTY_CELL(8));
    assertEquals(r3.getCell(9), cellMap2.get("test.boolean1"));
    assertEquals(r3.getCell(10), CellBean.EMPTY_CELL(10));
    assertEquals(r3.getCell(11), CellBean.EMPTY_CELL(11));
    assertEquals(r3.getCell(12), cellMap2.get("test.bigDecimal"));
    assertEquals(r3.getCell(13), cellMap2.get("test.localDate"));
    assertEquals(r3.getCell(14), cellMap2.get("test.localDateTime"));
  }
}
