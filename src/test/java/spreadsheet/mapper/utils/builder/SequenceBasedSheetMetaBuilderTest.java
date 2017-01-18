package spreadsheet.mapper.utils.builder;

import org.testng.annotations.Test;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static spreadsheet.mapper.AssertUtil.assertSheetMetaEquals;

/**
 * Created by hanwen on 2017/1/10.
 */
public class SequenceBasedSheetMetaBuilderTest {

  @Test
  public void testBuilder() {

    SequenceBasedSheetMetaBuilder builder = new SequenceBasedSheetMetaBuilder();

    builder
        .field("test.", "test.int1").header("test.int1").next()
        .field("test.", "test.int2").header("test.int2").next()
        .field("test.", "test.long1").header("test.long1").next()
        .field("test.", "test.long2").header("test.long2").next()
        .field("test.", "test.float1").header("test.float1").next()
        .field("test.", "test.float2").header("test.float2").next()
        .field("test.", "test.double1").header("test.double1").next()
        .field("test.", "test.double2").header("test.double2").next()
        .field("test.", "test.boolean1").header("test.boolean1").next()
        .field("test.", "test.boolean2").header("test.boolean2").next()
        .field("test.", "test.string").header("test.string").next()
        .field("test.", "test.bigDecimal").header("test.bigDecimal").next()
        .field("test.", "test.localDate").header("test.localDate").next()
        .field("test.", "test.localDateTime").header("test.localDateTime").next();

    SheetMeta expected = TestFactory.createSheetMeta(true);

    SheetMeta sheetMeta = builder.toSheetMeta();

    assertSheetMetaEquals(sheetMeta, expected);
  }

  @Test
  public void testBuilder2() {

    SequenceBasedSheetMetaBuilder builder = new SequenceBasedSheetMetaBuilder();

    SheetMeta sheetMeta = builder.field("test1").next().field("test2").next().field("test3").next().toSheetMeta();


    assertEquals(sheetMeta.getDataStartRowIndex(), 1);
    assertEquals(sheetMeta.getFieldMetas().size(), 3);
    for (FieldMeta fieldMeta : sheetMeta.getFieldMetas()) {
      assertEquals(fieldMeta.getHeaderMetas().size(), 0);
    }
  }

  @Test
  public void testBuilder3() {

    SequenceBasedSheetMetaBuilder builder = new SequenceBasedSheetMetaBuilder();

    SheetMeta sheetMeta = builder
        .field("test1").skip().header("test1").next()
        .skip()
        .field("test2").skip().header("test2").next()
        .toSheetMeta();

    assertEquals(sheetMeta.getDataStartRowIndex(), 3);
    assertEquals(sheetMeta.getFieldMetas().size(), 2);

    assertEquals(sheetMeta.getFieldMeta("test1").getColumnIndex(), 1);
    assertEquals(sheetMeta.getFieldMeta("test2").getColumnIndex(), 3);

    for (FieldMeta fieldMeta : sheetMeta.getFieldMetas()) {
      assertEquals(fieldMeta.getHeaderMetas().size(), 1);
      assertNotNull(fieldMeta.getHeaderMeta(2));
      assertEquals(fieldMeta.getHeaderMeta(2).getRowIndex(), 2);
    }

  }

  @Test
  public void testBuilder4() {

    SequenceBasedSheetMetaBuilder builder = new SequenceBasedSheetMetaBuilder();

    SheetMeta sheetMeta = builder
        .field("test1").headers("title1", "title2").skip().header("title3").next()
        .skip(2)
        .field("test2").headers("title1", "title2").next()
        .skip(2)
        .field("test3").headers("title1", "title2").skip().header("title3").next()
        .sheetName("testSheet")
        .dataStartRowIndex(5)
        .toSheetMeta();

    assertEquals(sheetMeta.getDataStartRowIndex(), 5);
    assertEquals(sheetMeta.getFieldMetas().size(), 3);

    FieldMeta f1 = sheetMeta.getFieldMeta("test1");
    assertEquals(f1.getColumnIndex(), 1);
    FieldMeta f2 = sheetMeta.getFieldMeta("test2");
    assertEquals(f2.getColumnIndex(), 4);
    FieldMeta f3 = sheetMeta.getFieldMeta("test3");
    assertEquals(f3.getColumnIndex(), 7);

    assertEquals(f1.getHeaderMetas().size(), 3);
    assertNotNull(f1.getHeaderMeta(1));
    assertNotNull(f1.getHeaderMeta(2));
    assertNull(f1.getHeaderMeta(3));
    assertNotNull(f1.getHeaderMeta(4));

    assertEquals(f2.getHeaderMetas().size(), 2);
    assertNotNull(f2.getHeaderMeta(1));
    assertNotNull(f2.getHeaderMeta(2));
  }

}