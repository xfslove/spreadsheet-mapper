package spreadsheet.mapper.model.msg;

import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by hanwen on 2017/1/3.
 */
public class MessageBean implements Message {

  private String messageWriteStrategy;

  private String errorMessage;

  private int sheetIndex;

  private Integer rowIndex;

  private Integer columnIndex;

  public MessageBean(String messageWriteStrategy, String errorMessage, int sheetIndex) {
    this(messageWriteStrategy, errorMessage, sheetIndex, null, null);
  }

  public MessageBean(String messageWriteStrategy, String errorMessage, int sheetIndex, Integer rowIndex, Integer columnIndex) {
    if (rowIndex == null ^ columnIndex == null) {
      throw new IllegalArgumentException("row index and column index must both null or both not null");
    }

    this.messageWriteStrategy = messageWriteStrategy;
    this.errorMessage = errorMessage;
    this.sheetIndex = sheetIndex;
    this.rowIndex = rowIndex;
    this.columnIndex = columnIndex;
  }

  @Override
  public String getMessageWriteStrategy() {
    return messageWriteStrategy;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public Integer getRowIndex() {
    return rowIndex;
  }

  @Override
  public Integer getColumnIndex() {
    return columnIndex;
  }

  @Override
  public String getMessage() {
    return errorMessage;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("sheetIndex", sheetIndex)
        .append("rowIndex", rowIndex)
        .append("columnIndex", columnIndex)
        .append("errorMessage", errorMessage)
        .append("messageWriteStrategy", messageWriteStrategy)
        .toString();
  }
}
