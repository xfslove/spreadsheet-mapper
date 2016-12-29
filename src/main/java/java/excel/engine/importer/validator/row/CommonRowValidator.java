package java.excel.engine.importer.validator.row;


import java.excel.engine.model.excel.Row;

import java.util.Set;
import java.util.function.Function;

/**
 * customer row validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class CommonRowValidator extends RowValidatorAdapter {

  private Function<Row, Boolean> validateResultGetter;

  public CommonRowValidator(Function<Row, Boolean> validateResultGetter, String errorMessage, Set<String> messageOnFields) {
    super(null, errorMessage, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonRowValidator(Function<Row, Boolean> validateResultGetter, Set<String> messageOnFields) {
    super(null,null, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  @Override
  protected boolean customValidate(Row row) {
    if (validateResultGetter == null) {
      return true;
    }
    return validateResultGetter.apply(row);
  }
}
