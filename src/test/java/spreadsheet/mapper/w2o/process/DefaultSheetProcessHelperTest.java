package spreadsheet.mapper.w2o.process;

import org.testng.annotations.Test;
import spreadsheet.mapper.AssertUtil;
import spreadsheet.mapper.TestBean;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.meta.SheetMetaBean;
import spreadsheet.mapper.w2o.process.listener.CellProcessListener;
import spreadsheet.mapper.w2o.process.listener.RowProcessListener;
import spreadsheet.mapper.w2o.process.listener.SheetProcessListener;
import spreadsheet.mapper.w2o.process.setter.BooleanSetter;
import spreadsheet.mapper.w2o.process.setter.LocalDateSetter;
import spreadsheet.mapper.w2o.process.setter.LocalDateTimeSetter;

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

    Counter counter = new Counter();
    SheetProcessHelper<TestBean> processor1 = new DefaultSheetProcessHelper<TestBean>()
        .setSheet(sheet).setSheetMeta(sheetMeta1).setObjectFactory(new TestBeanObjectFactory())
        .setSheetProcessorListener(new TestSheetProcessListener(counter))
        .setRowProcessorListener(new TestRowProcessListener(counter))
        .setCellProcessorListener(new TestCellProcessListener(counter));

    processor1.addFieldSetter(new LocalDateTimeSetter<TestBean>().pattern("yyyy-MM-dd HH:mm:ss").matchField("test.localDateTime"));
    processor1.addFieldSetter(new LocalDateSetter<TestBean>().pattern("yyyy-MM-dd").matchField("test.localDate"));
    processor1.addFieldSetter(new BooleanSetter<TestBean>().matchField("test.boolean1").toTrue("pass").toFalse("failure"));
    processor1.addFieldSetter(new BooleanSetter<TestBean>().toTrue("pass").toFalse("failure").matchField("test.boolean2"));

    List<TestBean> list1 = processor1.process();

    assertEquals(counter.hitTime(), 2 + 2 * 2 + 14 * 2 * 2);

    assertEquals(list1.size(), 2);

    AssertUtil.assertTestBean1Equals(list1.get(0));
    AssertUtil.assertTestBean2Equals(list1.get(1));

    SheetMeta sheetMeta2 = new SheetMetaBean(sheetMeta1.getDataStartRowIndex());

    SheetProcessHelper<TestBean> processor2 = new DefaultSheetProcessHelper<TestBean>()
        .setSheet(sheet).setSheetMeta(sheetMeta2).setObjectFactory(new TestBeanObjectFactory());

    List<TestBean> list2 = processor2.process();

    assertEquals(list2.size(), 2);
    for (TestBean testBean : list2) {
      AssertUtil.assertTestBeanNull(testBean);
    }
  }

  private class TestBeanObjectFactory implements ObjectFactory<TestBean> {

    @Override
    public TestBean create(Row row, SheetMeta sheetMeta) {
      return new TestBean();
    }
  }

  private class TestSheetProcessListener implements SheetProcessListener<TestBean> {

    private Counter counter;

    public TestSheetProcessListener(Counter counter) {
      this.counter = counter;
    }

    @Override
    public void before(Sheet sheet, SheetMeta sheetMeta) {
      counter.hit();
    }

    @Override
    public void after(List<TestBean> objects, Sheet sheet, SheetMeta sheetMeta) {
      counter.hit();
    }
  }

  private class TestRowProcessListener implements RowProcessListener<TestBean> {

    private Counter counter;

    public TestRowProcessListener(Counter counter) {
      this.counter = counter;
    }

    @Override
    public void before(TestBean object, Row row, SheetMeta sheetMeta) {
      counter.hit();
    }

    @Override
    public void after(TestBean object, Row row, SheetMeta sheetMeta) {
      counter.hit();
    }
  }

  private class TestCellProcessListener implements CellProcessListener<TestBean> {

    private Counter counter;

    public TestCellProcessListener(Counter counter) {
      this.counter = counter;
    }

    @Override
    public void before(TestBean object, Cell cell, FieldMeta fieldMeta) {
      counter.hit();
    }

    @Override
    public void after(TestBean object, Cell cell, FieldMeta fieldMeta) {
      counter.hit();
    }
  }

  private class Counter {
    private int count = 0;

    void hit() {
      count++;
    }

    int hitTime() {
      return count;
    }
  }
}