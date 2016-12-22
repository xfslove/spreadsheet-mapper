package me.excel.tools.setter;

import me.excel.tools.factory.TestPersonModel;
import me.excel.tools.model.excel.ExcelCellBean;
import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;

/**
 * Created by hanwen on 2016/12/22.
 */
public class BooleanValueSetterTest {

  @Test
  public void testSet() throws Exception {

    TestPersonModel model = new TestPersonModel();

    BooleanValueSetter booleanValueSetter = new BooleanValueSetter("person.male");
    ExcelCellBean cell = new ExcelCellBean(1, 1, "person.male", "0");
    booleanValueSetter.set(model, cell);

    assertEquals(model.isMale(), false);
  }

}