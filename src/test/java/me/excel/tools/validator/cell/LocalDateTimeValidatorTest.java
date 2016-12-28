package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LocalDateTimeValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    LocalDateTimeValidator localDateTimeValidator = new LocalDateTimeValidator("person.localDateTime", "yyyy-MM-dd HH:mm");

    CellBean cell4 = new CellBean(1, 1, "person.localDateTime", "");
    assertTrue(localDateTimeValidator.validate(cell4));

    CellBean cell = new CellBean(1, 1, "person.localDateTime", "1988-10-10 09:00:00");
    assertFalse(localDateTimeValidator.validate(cell));

    CellBean cell1 = new CellBean(1, 1, "person.localDateTime", "1988-10-10 09:00");
    assertTrue(localDateTimeValidator.validate(cell1));

    CellBean cell2 = new CellBean(1, 1, "person.localDateTime", "asdasd");
    assertFalse(localDateTimeValidator.validate(cell2));
  }

}