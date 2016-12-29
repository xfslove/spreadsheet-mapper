package java.excel.engine.importer.validator.cell;

import java.excel.engine.model.excel.CellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LocalDateValidatorTest {

  @Test
  public void testCustomValidate() throws Exception {

    LocalDateTimeValidator localDateTimeValidator = new LocalDateTimeValidator("person.localDate", "yyyy-MM-dd");

    CellBean cell4 = new CellBean(1, 1, "");
    cell4.setField("person.localDate");
    assertTrue(localDateTimeValidator.valid(cell4));

    CellBean cell = new CellBean(1, 1, "1988-10-10 09:00:00");
    cell.setField("person.localDate");
    assertFalse(localDateTimeValidator.valid(cell));

    CellBean cell1 = new CellBean(1, 1, "1988-10-10");
    cell1.setField("person.localDate");
    assertTrue(localDateTimeValidator.valid(cell1));

    CellBean cell2 = new CellBean(1, 1, "asdasd");
    cell2.setField("person.localDate");
    assertFalse(localDateTimeValidator.valid(cell2));
  }

}