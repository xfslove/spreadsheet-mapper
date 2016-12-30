package excel.engine.w2o.validator.cell;


import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;

import static excel.engine.util.BooleanUtils.isValidFalse;
import static excel.engine.util.BooleanUtils.isValidTrue;

/**
 * boolean validator
 * <p>
 * Created by hanwen on 15-12-18.
 */
public class BooleanValidator extends CellValidatorAdapter {

  public BooleanValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    return isValidTrue(cell.getValue()) || isValidFalse(cell.getValue());
  }

}
