package me.excel.tools.setter;

import me.excel.tools.factory.TestPersonModel;
import me.excel.tools.model.excel.ExcelCellBean;
import org.joda.time.LocalDate;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LocalDateTimeValueSetterTest {

  @Test
  public void testSet() throws Exception {

    TestPersonModel model = new TestPersonModel();

    LocalDateValueSetter localDateValueSetter = new LocalDateValueSetter("person.birthday", "yyyy-MM-dd");
    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.birthday", "1984-11-22");
    localDateValueSetter.set(model, cell);

    assertEquals(model.getBirthday(), new LocalDate(1984, 11, 22));
  }

}