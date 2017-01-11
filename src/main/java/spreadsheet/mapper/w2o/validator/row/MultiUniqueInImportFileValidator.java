package spreadsheet.mapper.w2o.validator.row;

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
public class MultiUniqueInImportFileValidator extends RowValidatorAdapter {

  // format: "field1-value1,field2-value2,..."
  private Set<String> rowValueHolder = new HashSet<>();

  private Set<String> matchFields = new HashSet<>();

  public MultiUniqueInImportFileValidator(String[] matchFields, String group, String errorMessage) {
    this(matchFields, group, errorMessage, matchFields);
  }

  public MultiUniqueInImportFileValidator(String[] matchFields, String group, String errorMessage, String[] messageOnFields) {
    this(matchFields, group, errorMessage, messageOnFields, null);
  }

  public MultiUniqueInImportFileValidator(String[] matchFields, String group, String errorMessage, String[] messageOnFields, String[] dependsOn) {
    super(group, errorMessage, messageOnFields, dependsOn);
    if (matchFields != null) {
      Collections.addAll(this.matchFields, matchFields);
    }
  }

  @Override
  protected boolean customValidate(Row row, SheetMeta sheetMeta) {

    List<String> holdStringList = new ArrayList<>();

    for (String field : matchFields) {
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
   * @param cell
   * @return
   */
  private String buildHoldString(FieldMeta fieldMeta, Cell cell) {
    return fieldMeta.getName() + Constants.NEGATIVE_SEPARATOR + cell.getValue();
  }
}
