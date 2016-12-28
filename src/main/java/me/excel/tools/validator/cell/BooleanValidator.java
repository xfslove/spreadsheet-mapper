package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.Cell;

import static me.excel.tools.BooleanTranslator.isValidFalse;
import static me.excel.tools.BooleanTranslator.isValidTrue;

/**
 * boolean validator
 * <p>
 * Created by hanwen on 15-12-18.
 */
public class BooleanValidator extends CellValidatorAdapter {

  public BooleanValidator(String matchField) {
    super(matchField, "格式应为: \'是\':true,t,是,yes,y,1; \'否\':false,f,否,no,n,0");
  }

  public BooleanValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(Cell cell) {
    return isValidTrue(cell.getValue()) || isValidFalse(cell.getValue());
  }

}
