package me.excel.tools.validator.data.cell;

import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class IntValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    IntValidator intValidator = new IntValidator("person.int");

    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.float", "");
    assertTrue(intValidator.validate(cell));

    ExcelCellBean cell2 = new ExcelCellBean(1, 1, "person.float", "asdasd");
    assertFalse(intValidator.validate(cell2));

    ExcelCellBean cell1 = new ExcelCellBean(1, 1, "person.float", "1");
    assertTrue(intValidator.validate(cell1));
  }

}