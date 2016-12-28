package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LongValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    LongValidator longValidator = new LongValidator("person.long");

    CellBean cell = new CellBean(1, 1, "person.long", "");
    assertTrue(longValidator.validate(cell));

    CellBean cell1 = new CellBean(1, 1, "person.long", "dasdasd");
    assertFalse(longValidator.validate(cell1));

    CellBean cell2 = new CellBean(1, 1, "person.long", "1");
    assertTrue(longValidator.validate(cell2));

  }

}