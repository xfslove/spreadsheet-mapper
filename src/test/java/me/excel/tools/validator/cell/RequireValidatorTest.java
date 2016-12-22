package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class RequireValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    RequireValidator requireValidator = new RequireValidator("person.name");

    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.name", "");
    assertFalse(requireValidator.validate(cell));

    ExcelCellBean cell1 = new ExcelCellBean(1, 1, "person.name", "dasdasd");
    assertTrue(requireValidator.validate(cell1));

    ExcelCellBean cell2 = new ExcelCellBean(1, 1, "person.name", "1");
    assertTrue(requireValidator.validate(cell2));

  }

}