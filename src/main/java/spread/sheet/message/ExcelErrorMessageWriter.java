package spread.sheet.message;

import spread.sheet.f2w.WorkbookReadException;
import spread.sheet.model.message.ErrorMessage;
import spread.sheet.w2f.WorkbookWriteException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by hanwen on 2017/1/3.
 */
public class ExcelErrorMessageWriter implements ErrorMessageWriter {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelErrorMessageWriter.class);

  private Map<String, MessageWriteStrategy> strategy2writeStrategy = new HashMap<>();

  /**
   * this will create a new excel workbook to write error messages
   */
  public ExcelErrorMessageWriter() {
    // default constructor
  }

  /**
   * this will copy a excel workbook from supplied input stream to write error messages
   *
   * @param inputStream auto close
   */
  public ExcelErrorMessageWriter(InputStream inputStream) {
    try (Workbook workbook = WorkbookFactory.create(inputStream)) {
      messageWriteStrategy(
          new SingleCommentInCellStrategy(workbook),
          new SingleTextBoxInSheetStrategy(workbook));
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookReadException(e);
    }
  }

  @Override
  public ErrorMessageWriter messageWriteStrategy(MessageWriteStrategy... messageWriteStrategies) {
    if (messageWriteStrategies == null) {
      return this;
    }
    for (MessageWriteStrategy messageWriteStrategy : messageWriteStrategies) {
      strategy2writeStrategy.put(messageWriteStrategy.getStrategy(), messageWriteStrategy);
    }
    return this;
  }

  @Override
  public void write(Collection<ErrorMessage> errorMessages, OutputStream outputStream) {
    Map<String, Collection<ErrorMessage>> messageWriteStrategyMap = buildMessageWriteStrategyMap(errorMessages);
    for (String writeStrategy : messageWriteStrategyMap.keySet()) {
      MessageWriteStrategy messageWriteStrategy = strategy2writeStrategy.get(writeStrategy);
      if (messageWriteStrategy == null) {
        throw new WorkbookWriteException("no message write helper of [" + writeStrategy + "]");
      }

      messageWriteStrategy.write(outputStream, messageWriteStrategyMap.get(writeStrategy));
    }
  }

  private Map<String, Collection<ErrorMessage>> buildMessageWriteStrategyMap(Collection<ErrorMessage> errorMessages) {
    Map<String, Collection<ErrorMessage>> messageWriteStrategyMap = new HashMap<>();

    for (ErrorMessage errorMessage : errorMessages) {
      String messageWriteStrategy = errorMessage.getMessageWriteStrategy();

      if (!messageWriteStrategyMap.containsKey(messageWriteStrategy)) {
        messageWriteStrategyMap.put(messageWriteStrategy, new ArrayList<ErrorMessage>());
      }
      messageWriteStrategyMap.get(messageWriteStrategy).add(errorMessage);
    }

    return messageWriteStrategyMap;
  }
}
