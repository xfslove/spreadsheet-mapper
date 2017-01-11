package spreadsheet.mapper.w2o.validator.cell;


import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.core.Cell;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * boolean validator
 * <p>
 * Created by hanwen on 15-12-18.
 */
public class BooleanValidator extends CellValidatorAdapter {

  private Set<String> supportedTrueStrings = new HashSet<>();

  private Set<String> supportedFalseStrings = new HashSet<>();

  public BooleanValidator(String[] supportedTrueStrings, String[] supportedFalseStrings, String matchField, String errorMessage) {
    this(supportedTrueStrings, supportedFalseStrings, matchField, errorMessage, null);
  }

  public BooleanValidator(String[] supportedTrueStrings, String[] supportedFalseStrings, String matchField, String errorMessage, String[] dependsOn) {
    this(supportedTrueStrings, supportedFalseStrings, matchField, matchField, errorMessage, dependsOn);
  }

  public BooleanValidator(String[] supportedTrueStrings, String[] supportedFalseStrings, String group, String matchField, String errorMessage, String[] dependsOn) {
    this(supportedTrueStrings, supportedFalseStrings, group, matchField, errorMessage, matchField, dependsOn);
  }

  public BooleanValidator(String[] supportedTrueStrings, String[] supportedFalseStrings, String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
    if (supportedTrueStrings != null) {
      Collections.addAll(this.supportedTrueStrings, supportedTrueStrings);
    }
    if (supportedFalseStrings != null) {
      Collections.addAll(this.supportedFalseStrings, supportedFalseStrings);
    }
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    String value = cell.getValue();
    return supportedTrueStrings.contains(value) || supportedFalseStrings.contains(value);
  }

}
