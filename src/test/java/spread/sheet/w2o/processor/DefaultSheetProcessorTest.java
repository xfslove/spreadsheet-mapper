package spread.sheet.w2o.processor;

import org.testng.annotations.Test;
import spread.sheet.AssertUtil;
import spread.sheet.TestBean;
import spread.sheet.TestFactory;
import spread.sheet.model.core.Row;
import spread.sheet.model.core.Sheet;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.model.meta.SheetMetaBean;
import spread.sheet.w2o.setter.BooleanValueSetter;
import spread.sheet.w2o.setter.LocalDateTimeValueSetter;
import spread.sheet.w2o.setter.LocalDateValueSetter;

import java.util.List;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2017/1/5.
 */
public class DefaultSheetProcessorTest {

  @Test
  public void testProcess() throws Exception {

    Sheet sheet = TestFactory.createSheet();
    SheetMeta sheetMeta1 = TestFactory.createSheetMeta(true);

    SheetProcessor<TestBean> processor1 = new DefaultSheetProcessor<TestBean>()
        .sheet(sheet).sheetMeta(sheetMeta1).objectFactory(new TestBeanObjectFactory());

    processor1.fieldValueSetter(
        new LocalDateTimeValueSetter("yyyy-MM-dd HH:mm:ss", "test.localDateTime"),
        new LocalDateValueSetter("yyyy-MM-dd", "test.localDate"),
        new BooleanValueSetter(new String[] {"pass"}, new String[] {"failure"}, "test.boolean1"),
        new BooleanValueSetter(new String[] {"pass"}, new String[] {"failure"}, "test.boolean2")
    );

    List<TestBean> list1 = processor1.process();

    assertEquals(list1.size(), 2);

    AssertUtil.assertTestBean1Equals(list1.get(0));
    AssertUtil.assertTestBean2Equals(list1.get(1));

    SheetMeta sheetMeta2 = new SheetMetaBean(sheetMeta1.getDataStartRowIndex());

    SheetProcessor<TestBean> processor2 = new DefaultSheetProcessor<TestBean>()
        .sheet(sheet).sheetMeta(sheetMeta2).objectFactory(new TestBeanObjectFactory());

    List<TestBean> list2 = processor2.process();

    assertEquals(list2.size(), 2);
    for (TestBean testBean : list2) {
      AssertUtil.assertTestBeanNull(testBean);
    }
  }

  private class TestBeanObjectFactory implements ObjectFactory<TestBean> {

    @Override
    public TestBean create(Row row) {
      return new TestBean();
    }
  }

}