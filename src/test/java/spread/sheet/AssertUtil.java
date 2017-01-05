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

    assertEquals(r2.getCell(1).getValue(), cellMap1.get("test.int1").getValue());
    assertEquals(r2.getCell(2).getValue(), cellMap1.get("test.int2").getValue());
    assertEquals(r2.getCell(3).getValue(), cellMap1.get("test.long1").getValue());
    assertEquals(r2.getCell(4).getValue(), cellMap1.get("test.long2").getValue());
    assertEquals(r2.getCell(5).getValue(), cellMap1.get("test.float1").getValue());
    assertEquals(r2.getCell(6).getValue(), cellMap1.get("test.float2").getValue());
    assertEquals(r2.getCell(7).getValue(), cellMap1.get("test.double1").getValue());
    assertEquals(r2.getCell(8).getValue(), cellMap1.get("test.double2").getValue());
    assertEquals(r2.getCell(9).getValue(), cellMap1.get("test.boolean1").getValue());
    assertEquals(r2.getCell(10).getValue(), cellMap1.get("test.boolean2").getValue());
    assertEquals(r2.getCell(11).getValue(), cellMap1.get("test.string").getValue());
    assertEquals(r2.getCell(12).getValue(), cellMap1.get("test.bigDecimal").getValue());
    assertEquals(r2.getCell(13).getValue(), cellMap1.get("test.localDate").getValue());
    assertEquals(r2.getCell(14).getValue(), cellMap1.get("test.localDateTime").getValue());

    Map<String, Cell> cellMap2 = TestFactory.createCellMap2();
    assertEquals(r3.getCell(1).getValue(), cellMap2.get("test.int1").getValue());
    assertEquals(r3.getCell(2).getValue(), new CellBean().getValue());
    assertEquals(r3.getCell(3).getValue(), cellMap2.get("test.long1").getValue());
    assertEquals(r3.getCell(4).getValue(), new CellBean().getValue());
    assertEquals(r3.getCell(5).getValue(), cellMap2.get("test.float1").getValue());
    assertEquals(r3.getCell(6).getValue(), new CellBean().getValue());
    assertEquals(r3.getCell(7).getValue(), cellMap2.get("test.double1").getValue());
    assertEquals(r3.getCell(8).getValue(), new CellBean().getValue());
    assertEquals(r3.getCell(9).getValue(), cellMap2.get("test.boolean1").getValue());
    assertEquals(r3.getCell(10).getValue(), new CellBean().getValue());
    assertEquals(r3.getCell(11).getValue(), cellMap2.get("test.string").getValue());
    assertEquals(r3.getCell(12).getValue(), cellMap2.get("test.bigDecimal").getValue());
    assertEquals(r3.getCell(13).getValue(), cellMap2.get("test.localDate").getValue());
    assertEquals(r3.getCell(14).getValue(), cellMap2.get("test.localDateTime").getValue());
  }
}
