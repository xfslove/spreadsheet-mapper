package excel.engine.importer.validator.cell;


import excel.engine.model.excel.Cell;

import static excel.engine.util.BooleanUtils.isValidFalse;
import static excel.engine.util.BooleanUtils.isValidTrue;

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
