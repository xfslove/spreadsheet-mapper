package spread.sheet.o2w.composer;

import org.testng.annotations.Test;
import spread.sheet.AssertUtil;
import spread.sheet.TestBean;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Sheet;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.o2w.extractor.BooleanExtractor;
import spread.sheet.o2w.extractor.LocalDateTimeExtractor;
import spread.sheet.o2w.extractor.PlainNumberExtractor;

import java.util.Arrays;
import java.util.List;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2017/1/5.
 */
public class DefaultSheetComposerTest {

  @Test
  public void testCompose() throws Exception {

    SheetMeta sheetMeta1 = TestFactory.createSheetMeta(true);

    TestBean testBean1 = TestFactory.createBean1();
    TestBean testBean2 = TestFactory.createBean2();

    List<TestBean> data = Arrays.asList(testBean1, testBean2);

    SheetComposer<TestBean> sheetComposer1 = new DefaultSheetComposer<TestBean>().sheetMeta(sheetMeta1).data(data);
    addExtractor(sheetComposer1);

    Sheet sheet1 = sheetComposer1.compose();

    AssertUtil.assertSheetEquals(sheet1, true);

    SheetMeta sheetMeta2 = TestFactory.createSheetMeta(false);

    SheetComposer<TestBean> sheetComposer2 = new DefaultSheetComposer<TestBean>().sheetMeta(sheetMeta2).data(data);
    addExtractor(sheetComposer2);

    Sheet sheet2 = sheetComposer2.compose();

    AssertUtil.assertSheetEquals(sheet2, false);

    SheetComposer<TestBean> sheetComposer3 = new DefaultSheetComposer<TestBean>().sheetMeta(sheetMeta1);

    Sheet sheet3 = sheetComposer3.compose();
    assertEquals(sheet3.sizeOfRows(), 1);
    AssertUtil.assertHeaderRowEquals(sheet3.getRow(1), true);
  }

  private void addExtractor(SheetComposer sheetComposer) {
    sheetComposer.fieldValueExtractor(
        new PlainNumberExtractor("test.int1"),
        new PlainNumberExtractor("test.int2"),
        new PlainNumberExtractor("test.long1"),
        new PlainNumberExtractor("test.long2"),
        new PlainNumberExtractor("test.float1"),
        new PlainNumberExtractor("test.float2"),
        new PlainNumberExtractor("test.double1"),
        new PlainNumberExtractor("test.double2"),
        new BooleanExtractor("test.boolean1", "pass", "failure"),
        new BooleanExtractor("test.boolean2", "pass", "failure"),
        new LocalDateTimeExtractor("yyyy-MM-dd HH:mm:ss", "test.localDateTime")
    );
  }
}