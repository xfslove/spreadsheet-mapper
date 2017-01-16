package spreadsheet.mapper.w2o.validation.validator.cell;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * <pre>
 * cell value validator adapter, using builder pattern, easy implements customer cell validator extends this.
 * extends this validator will skip custom valid when cell value is blank (default blank value means no need valid).
 * </pre>
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public abstract class CellValidatorAdapter<T extends CellValidatorAdapter<T>> {

  private String group;

  private Set<String> dependsOn = new LinkedHashSet<>();

  private String matchField;

  private String errorMessage;

  private String messageOnField;

  /**
   * @param matchField {@link CellValidator#getMatchField()}
   * @return {@link T}
   */
  public T matchField(String matchField) {
    this.matchField = matchField;
    return getThis();
  }

  /**
   * @param errorMessage {@link CellValidator#getErrorMessage()}
   * @return {@link T}
   */
  public T errorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
    return getThis();
  }

  /**
   * if empty default is {@link #getMatchField()}
   *
   * @param messageOnField {@link CellValidator#getMessageOnField()}
   * @return {@link T}
   */
  public T messageOnField(String messageOnField) {
    this.messageOnField = messageOnField;
    return getThis();
  }

  /**
   * @param dependsOn {@link CellValidator#getDependsOn()}
   * @return {@link T}
   */
  public T dependsOn(String... dependsOn) {
    if (dependsOn == null) {
      return getThis();
    }
    Collections.addAll(this.dependsOn, dependsOn);
    return getThis();
  }

  /**
   * if empty default is {@link #getMatchField()}
   *
   * @param group {@link CellValidator#getGroup()}
   * @return {@link T}
   */
  public T group(String group) {
    this.group = group;
    return getThis();
  }

  /**
   * finish build a cell validator from supplied properties
   *
   * @return {@link CellValidator}
   */
  public CellValidator end() {

    return new CellValidator() {

      @Override
      public boolean valid(Cell cell, FieldMeta fieldMeta) {
        return CellValidatorAdapter.this.valid(cell, fieldMeta);
      }

      @Override
      public String getMatchField() {
        return CellValidatorAdapter.this.getMatchField();
      }

      @Override
      public String getMessageOnField() {
        if (StringUtils.isBlank(CellValidatorAdapter.this.getMessageOnField())) {
          return CellValidatorAdapter.this.getMatchField();
        }
        return CellValidatorAdapter.this.getMessageOnField();
      }

      @Override
      public String getGroup() {
        if (StringUtils.isBlank(CellValidatorAdapter.this.getGroup())) {
          return CellValidatorAdapter.this.getMatchField();
        }
        return CellValidatorAdapter.this.getGroup();
      }

      @Override
      public Set<String> getDependsOn() {
        return CellValidatorAdapter.this.getDependsOn();
      }

      @Override
      public String getErrorMessage() {
        return CellValidatorAdapter.this.getErrorMessage();
      }
    };

  }

  protected abstract T getThis();

  protected abstract boolean customValid(Cell cell, FieldMeta fieldMeta);

  protected boolean valid(Cell cell, FieldMeta fieldMeta) {
    return StringUtils.isBlank(cell.getValue()) || customValid(cell, fieldMeta);
  }

  /*=====================
    for customer access
   =====================*/
  protected String getGroup() {
    return group;
  }

  public String getMatchField() {
    return matchField;
  }

  protected Set<String> getDependsOn() {
    return dependsOn;
  }

  protected String getErrorMessage() {
    return errorMessage;
  }

  protected String getMessageOnField() {
    return messageOnField;
  }
}
