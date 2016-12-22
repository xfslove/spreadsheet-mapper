package me.excel.tools.setter;

import me.excel.tools.factory.TestPersonModel;
import me.excel.tools.model.excel.ExcelCellBean;
import org.joda.time.LocalDateTime;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LocalDateValueSetterTest {

  @Test
  public void testSet() throws Exception {

    TestPersonModel model = new TestPersonModel();

    LocalDateTimeValueSetter localDateTimeValueSetter = new LocalDateTimeValueSetter("person.registerTime", "yyyy-MM-dd HH:mm:ss");
    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.registerTime", "2000-01-01 00:00:00");
    localDateTimeValueSetter.set(model, cell);

    assertEquals(model.getRegisterTime(), new LocalDateTime(2000, 1, 1, 0, 0, 0));

  }

}