package spread.sheet;

import org.joda.time.LocalDate;
import org.joda.time.LocalDateTime;
import spread.sheet.model.core.*;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.meta.FieldMetaBean;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.model.meta.SheetMetaBean;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Created by hanwen on 2017/1/4.
 */
public class TestFactory {

  public static TestBean create1() {

    TestBean testBean = new TestBean();

    testBean.setInt1(10000);
    testBean.setInt2(20000);

    testBean.setLong1(10000000000000L);
    testBean.setLong2(20000000000000L);

    testBean.setFloat1(0.001F);
    testBean.setFloat2(0.00000002F);

    testBean.setDouble1(0.00000000000000000001D);
    testBean.setDouble2(0.00000000000000000002D);

    testBean.setBoolean1(true);
    testBean.setBoolean2(false);

    testBean.setString("Scarlett Johansson");

    testBean.setBigDecimal(BigDecimal.valueOf(0.00000000000000000001D));

    testBean.setLocalDate(new LocalDate(1984, 11, 22));

    testBean.setLocalDateTime(new LocalDateTime(1984, 11, 22, 0, 0, 0));

    return testBean;
  }

  public static TestBean create2() {

    TestBean testBean = new TestBean();

    testBean.setInt1(1);

    testBean.setLong1(1L);

    testBean.setFloat1(0.1F);

    testBean.setDouble1(0.00001D);

    testBean.setString("Scarlett Johansson");

    testBean.setBoolean1(false);

    return testBean;
  }

  public static Map<String, FieldMeta> createFieldMetaMap() {

    Map<String, FieldMeta> fieldMetaMap = new LinkedHashMap<>();

    fieldMetaMap.put("test.int1", new FieldMetaBean("test.int1", 1));
    fieldMetaMap.put("test.int2", new FieldMetaBean("test.int2", 2));
    fieldMetaMap.put("test.long1", new FieldMetaBean("test.long1", 3));
    fieldMetaMap.put("test.long2", new FieldMetaBean("test.long2", 4));
    fieldMetaMap.put("test.float1", new FieldMetaBean("test.float1", 5));
    fieldMetaMap.put("test.float2", new FieldMetaBean("test.float2", 6));
    fieldMetaMap.put("test.double1", new FieldMetaBean("test.double1", 7));
    fieldMetaMap.put("test.double2", new FieldMetaBean("test.double2", 8));
    fieldMetaMap.put("test.boolean1", new FieldMetaBean("test.boolean1", 9));
    fieldMetaMap.put("test.boolean2", new FieldMetaBean("test.boolean2", 10));
    fieldMetaMap.put("test.string", new FieldMetaBean("test.string", 11));
    fieldMetaMap.put("test.bigDecimal", new FieldMetaBean("test.bigDecimal", 12));
    fieldMetaMap.put("test.localDate", new FieldMetaBean("test.localDate", 13));
    fieldMetaMap.put("test.localDateTime", new FieldMetaBean("test.localDateTime", 14));

    return fieldMetaMap;
  }

  public static Map<String, Cell> createCellMap1() {

    Map<String, Cell> cellMap = new LinkedHashMap<>();

    cellMap.put("test.int1", new CellBean("10000"));
    cellMap.put("test.int2", new CellBean("-20000"));
    cellMap.put("test.long1", new CellBean("10000000000000"));
    cellMap.put("test.long2", new CellBean("20000000000000"));
    cellMap.put("test.float1", new CellBean("0.001"));
    cellMap.put("test.float2", new CellBean("0.00000002"));
    cellMap.put("test.double1", new CellBean("0.00000000000000000001"));
    cellMap.put("test.double2", new CellBean("0.00000000000000000002"));
    cellMap.put("test.boolean1", new CellBean("pass"));
    cellMap.put("test.boolean2", new CellBean("failure"));
    cellMap.put("test.string", new CellBean("Scarlett Johansson"));
    cellMap.put("test.bigDecimal", new CellBean("1E-20"));
    cellMap.put("test.localDate", new CellBean("1984-11-22"));
    cellMap.put("test.localDateTime", new CellBean("1984-11-22 00:00:00"));

    return cellMap;
  }

  public static Map<String, Cell> createCellMap2() {

    Map<String, Cell> cellMap = new LinkedHashMap<>();

    cellMap.put("test.int1", new CellBean("dasdasd"));
    cellMap.put("test.int2", new CellBean(""));
    cellMap.put("test.long1", new CellBean("afsdfasdf"));
    cellMap.put("test.long2", new CellBean(""));
    cellMap.put("test.float1", new CellBean("0.asfadsf"));
    cellMap.put("test.float2", new CellBean(""));
    cellMap.put("test.double1", new CellBean("0.345dfasd"));
    cellMap.put("test.double2", new CellBean(""));
    cellMap.put("test.boolean1", new CellBean("t"));
    cellMap.put("test.boolean2", new CellBean(""));
    cellMap.put("test.string", new CellBean("Scarlett Johansson"));
    cellMap.put("test.bigDecimal", new CellBean("0.00000000000000000001"));
    cellMap.put("test.localDate", new CellBean("1984/11/22"));
    cellMap.put("test.localDateTime", new CellBean("fsadfsadf"));

    return cellMap;
  }

  public static Sheet createSheet() {

    Sheet sheet = new SheetBean();

    Row r1 = new RowBean();
    Row r2 = new RowBean();
    Row r3 = new RowBean();
    sheet.addRow(r1);
    sheet.addRow(r2);
    sheet.addRow(r3);

    Map<String, FieldMeta> fieldMetaMap = createFieldMetaMap();
    for (FieldMeta fieldMeta : fieldMetaMap.values()) {
      r1.addCell(new CellBean(fieldMeta.getName()));
    }

    Map<String, Cell> cellMap1 = createCellMap1();
    for (Cell cell : cellMap1.values()) {
      r2.addCell(cell);
    }
    Map<String, Cell> cellMap2 = createCellMap2();
    for (Cell cell : cellMap2.values()) {
      r3.addCell(cell);
    }

    return sheet;
  }

  public static Workbook createWorkbook() {
    Workbook workbook = new WorkbookBean();

    workbook.addSheet(createSheet());

    return workbook;
  }

  public static SheetMeta createSheetMeta() {

    SheetMeta sheetMeta = new SheetMetaBean(2);
    Map<String, FieldMeta> fieldMetaMap = createFieldMetaMap();
    for (FieldMeta fieldMeta : fieldMetaMap.values()) {
      sheetMeta.addFieldMeta(fieldMeta);
    }

    return sheetMeta;
  }
}
