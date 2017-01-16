package spreadsheet.mapper.w2o.process;

import org.testng.annotations.Test;
import spreadsheet.mapper.AssertUtil;
import spreadsheet.mapper.TestBean;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.meta.SheetMetaBean;
import spreadsheet.mapper.w2o.process.factory.ObjectFactory;
import spreadsheet.mapper.w2o.process.setter.BooleanSetter;
import spreadsheet.mapper.w2o.process.setter.LocalDateTimeSetter;
import spreadsheet.mapper.w2o.process.setter.LocalDateSetter;

import java.util.List;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2017/1/5.
 */
public class DefaultSheetProcessHelperTest {

  @Test
  public void testProcess() throws Exception {

    Sheet sheet = TestFactory.createSheet();
    SheetMeta sheetMeta1 = TestFactory.createSheetMeta(true);

    SheetProcessHelper<TestBean> processor1 = new DefaultSheetProcessHelper<TestBean>()
        .sheet(sheet).sheetMeta(sheetMeta1).objectFactory(new TestBeanObjectFactory());

    processor1.fieldSetters(
        new LocalDateTimeSetter<TestBean>().pattern("yyyy-MM-dd HH:mm:ss").matchField("test.localDateTime"),
        new LocalDateSetter<TestBean>().pattern("yyyy-MM-dd").matchField("test.localDate"),
        new BooleanSetter<TestBean>().matchField("test.boolean1").toTrue("pass").toFalse("failure"),
        new BooleanSetter<TestBean>().toTrue("pass").toFalse("failure").matchField("test.boolean2")
    );

    List<TestBean> list1 = processor1.process();

    assertEquals(list1.size(), 2);

    AssertUtil.assertTestBean1Equals(list1.get(0));
    AssertUtil.assertTestBean2Equals(list1.get(1));

    SheetMeta sheetMeta2 = new SheetMetaBean(sheetMeta1.getDataStartRowIndex());

    SheetProcessHelper<TestBean> processor2 = new DefaultSheetProcessHelper<TestBean>()
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