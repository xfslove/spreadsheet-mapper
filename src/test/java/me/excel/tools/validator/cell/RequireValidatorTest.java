package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class RequireValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    RequireValidator requireValidator = new RequireValidator("person.name");

    CellBean cell = new CellBean(1, 1, "person.name", "");
    assertFalse(requireValidator.validate(cell));

    CellBean cell1 = new CellBean(1, 1, "person.name", "dasdasd");
    assertTrue(requireValidator.validate(cell1));

    CellBean cell2 = new CellBean(1, 1, "person.name", "1");
    assertTrue(requireValidator.validate(cell2));

  }

}