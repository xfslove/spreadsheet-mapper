package spreadsheet.mapper.w2o.validation.validator.row;


import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.w2o.validation.WorkbookValidateException;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * row values validator adapter, using builder pattern, easy implements customer row validator extends this.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public abstract class RowValidatorAdapter<T extends RowValidatorAdapter<T>> {

  private String group;

  private Set<String> dependsOn = new LinkedHashSet<>();

  private String errorMessage;

  private Set<String> messageOnFields = new HashSet<>();

  /**
   * @param errorMessage {@link RowValidator#getErrorMessage()}
   * @return {@link T}
   */
  public T errorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
    return getThis();
  }

  /**
   * @param messageOnFields {@link RowValidator#getMessageOnFields()}
   * @return {@link T}
   */
  public T messageOnFields(String... messageOnFields) {
    if (messageOnFields == null) {
      return getThis();
    }
    Collections.addAll(this.messageOnFields, messageOnFields);
    return getThis();
  }

  /**
   * @param dependsOn {@link RowValidator#getDependsOn()}
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
   * @param group {@link RowValidator#getGroup()}
   * @return {@link T}
   */
  public T group(String group) {
    this.group = group;
    return getThis();
  }

  /**
   * finish build a row validator from supplied properties
   *
   * @return {@link RowValidator}
   */
  public RowValidator end() {

    return new RowValidator() {
      @Override
      public boolean valid(Row row, SheetMeta sheetMeta) {
        return customValid(row, sheetMeta);
      }

      @Override
      public Set<String> getMessageOnFields() {
        return RowValidatorAdapter.this.getMessageOnFields();
      }

      @Override
      public String getGroup() {
        if (StringUtils.isBlank(group)) {
          throw new WorkbookValidateException("row validator group must be set");
        }
        return group;
      }

      @Override
      public Set<String> getDependsOn() {
        return dependsOn;
      }

      @Override
      public String getErrorMessage() {
        return RowValidatorAdapter.this.getErrorMessage();
      }
    };
  }

  protected abstract boolean customValid(Row row, SheetMeta sheetMeta);

  protected abstract T getThis();

  /*=====================
    from customer access
   =====================*/
  protected Set<String> getMessageOnFields() {
    return messageOnFields;
  }

  protected String getErrorMessage() {
    return errorMessage;
  }
}
