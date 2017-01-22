package spreadsheet.mapper.w2o.validation.validator.cell.buildin;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.validation.validator.cell.CustomSingleCellValidatorAdapter;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * boolean validator
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class BooleanValidator extends CustomSingleCellValidatorAdapter<BooleanValidator> {

  private Set<String> trueStrings = new HashSet<>();

  private Set<String> falseStrings = new HashSet<>();

  public BooleanValidator supportedTrue(String... trueStrings) {
    if (trueStrings == null) {
      return getThis();
    }
    Collections.addAll(this.trueStrings, trueStrings);
    return getThis();
  }

  public BooleanValidator supportedFalse(String... falseStrings) {
    if (falseStrings == null) {
      return getThis();
    }
    Collections.addAll(this.falseStrings, falseStrings);
    return getThis();
  }

  @Override
  protected BooleanValidator getThis() {
    return this;
  }

  @Override
  protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
    String value = cell.getValue();
    return trueStrings.contains(value) || falseStrings.contains(value);
  }
}
