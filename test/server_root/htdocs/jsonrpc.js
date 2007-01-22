JsonRpcRequestId = 1;

JsonRpcTransaction = Class.create();
Object.extend(JsonRpcTransaction.prototype,
{
    initialize: function(serviceUrl, methodName, params, asynchronous) {
	this.serviceUrl = serviceUrl;
	this.methodName = methodName;
	this.params = params;
	this.reply = null;
	this.replyReady = 0;
	this.callbacks = [];
	this.request =
	    new Ajax.Request(serviceUrl,
			     { method: 'post',
			       requestHeaders: ['Content-type', 'application/json',
						'Accept', 'application/json'],
			       postBody: JSON.stringify({ version: "1.1",
							  id: JsonRpcRequestId++,
							  method: methodName,
							  params: params }),
			       asynchronous: !!asynchronous,
			       onComplete: this.receiveReply.bind(this) });
	if (!asynchronous) {
	    this.receiveReply(this.request);
	}
    },

    receiveReply: function(ajaxRequest) {
	var response = JSON.parse(ajaxRequest.responseText);
	if (response.error) {
	    throw response.error;
	} else {
	    var reply = response.result;
	    this.reply = reply;
	    this.replyReady = 1;
	    this.callbacks.each(function (cb) { cb(reply); });
	}
    },

    addCallback: function(cb) {
	this.callbacks.push(cb);
	if (this.replyReady) {
	    cb(this.reply);
	}
	return this;
    }
});

JsonRpcService = Class.create();
Object.extend(JsonRpcService.prototype,
{
    initialize: function(serviceUrl) {
	this.serviceUrl = serviceUrl;
	var txn = new JsonRpcTransaction(serviceUrl,
					 "system.describe",
					 [],
					 false);
	this.serviceDescription = txn.reply;
	var svc = this;
	this.serviceDescription.procs.each(function (desc) {
					       svc[desc.name] = svc.makeGenericProxy(desc);
					   });
    },

    makeGenericProxy: function(desc) {
	return function () {
	    var actuals = $A(arguments);
	    return new JsonRpcTransaction(this.smd.serviceURL,
					  desc.name,
					  actuals);
	};
    }
});
