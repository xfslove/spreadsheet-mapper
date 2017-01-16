package spreadsheet.mapper.w2o.validation.validator.row;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.Constants;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;

import java.util.*;

/**
 * <pre>
 * value union unique validator, it useful when you want valid some cells value union unique.
 *
 * eg:
 * if you excel files has person.idCardNumber and person.idCardType, you will want check if person's identify unique,
 * when the excel files has duplicate person identify this validator will get false.
 * </pre>
 * Created by hanwen on 2016/12/1.
 */
public class MultiUniqueValidator extends RowValidatorAdapter<MultiUniqueValidator> {

  // format: "field1-value1,field2-value2,..."
  private Set<String> rowValueHolder = new HashSet<>();

  private Set<String> multiUniqueFields = new HashSet<>();

  public MultiUniqueValidator multiUniqueFields(String... multiUniqueFields) {
    if (multiUniqueFields == null) {
      return getThis();
    }
    Collections.addAll(this.multiUniqueFields, multiUniqueFields);
    return getThis();
  }

  @Override
  public Set<String> getMessageOnFields() {
    Set<String> messageOnFields = super.getMessageOnFields();

    if (CollectionUtils.isEmpty(messageOnFields)) {
      return multiUniqueFields;
    }
    return messageOnFields;
  }

  @Override
  protected MultiUniqueValidator getThis() {
    return this;
  }

  @Override
  protected boolean customValid(Row row, SheetMeta sheetMeta) {

    List<String> holdStringList = new ArrayList<>();

    for (String field : multiUniqueFields) {
      FieldMeta fieldMeta = sheetMeta.getFieldMeta(field);
      Cell cell = row.getCell(fieldMeta.getColumnIndex());
      holdStringList.add(buildHoldString(fieldMeta, cell));
    }

    String holdValue = StringUtils.join(holdStringList, Constants.COMMA_SEPARATOR);

    if (rowValueHolder.contains(holdValue)) {
      return false;
    }

    rowValueHolder.add(holdValue);
    return true;
  }

  /**
   * build cache string as "field-value"
   *
   * @param fieldMeta {@link FieldMeta}
   * @param cell      {@link Cell}
   * @return hold string
   */
  protected String buildHoldString(FieldMeta fieldMeta, Cell cell) {
    return fieldMeta.getName() + Constants.NEGATIVE_SEPARATOR + cell.getValue();
  }
}
